#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/selection_tree.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The block instance `block` lowered from its own elaboration, its label
// settled on the scope itself (LRM 27.6): the body it is among the unit's, and
// how it builds the blocks it holds in turn. A name the body reaches outward
// resolves through the scopes holding the block.
auto LowerGenerateBlock(
    UnitLowerer& unit_lowerer, const slang::ast::Scope& block,
    std::string_view source_name, const WalkFrame& frame)
    -> diag::Result<hir::BlockConstruction> {
  StructuralScopeLowerer body(unit_lowerer, block);
  const slang::ast::Scope& holder = *block.asSymbol().getParentScope();
  auto lowered = body.Run(frame.WithDeclaringScope(
      unit_lowerer.DeclaringScopeChain(holder), &holder));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  lowered->scope.source_name = NameInAPath(source_name, block.getCompilation());
  return hir::BlockConstruction{
      .body = unit_lowerer.InternBlockBody(std::move(lowered->scope)),
      .built = std::move(lowered->construction)};
}

// Whether the loop the source wrote survived elaboration as something a
// construction can run: a block it counted out declares the index it is built
// with, it is named, and it has the three expressions that carry the index
// from one block to the next. Every loop the front end elaborated without an
// error has all of these. How the step is written is not among the
// conditions, because every form LRM 27.4 admits for one states where the
// index goes next and is carried down as written. Nor is how many blocks it
// counted: a loop that counted none is the same loop run no times.
auto LoopSurvivedElaboration(const slang::ast::GenerateBlockArraySymbol& array)
    -> bool {
  return array.valid &&
         std::ranges::all_of(
             array.entries,
             [](const slang::ast::GenerateBlockSymbol* entry) {
               return LoopIndexParameterOf(*entry) != nullptr;
             }) &&
         !array.name.empty() && array.loopVariable != nullptr &&
         array.initialExpression != nullptr &&
         array.stopExpression != nullptr && array.iterExpression != nullptr;
}

// The loop itself, for a generate a construction counts out: the index
// declared by the scope holding the generate, the three expressions that reach
// it the way any name reaches a declaration -- the condition reading it, the
// step writing it -- and the index as each construction hands it to the block.
// The variable is the holding scope's own, so that read reaches it over the
// route that climbs no edges.
auto BuildTheLoop(
    StructuralScopeLowerer& lowerer,
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::BlocksRepeat> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const slang::ast::ValueSymbol& loop_variable = *array.loopVariable;
  const auto span = frontend::PointSpanOf(loop_variable.location);

  auto index_type = unit_lowerer.InternType(loop_variable.getType(), span);
  if (!index_type) return std::unexpected(std::move(index_type.error()));
  const hir::StructuralDataObjectId variable =
      frame.current_structural_scope->structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{loop_variable.name},
              .type = *index_type,
              .kind = hir::StructuralGenvarDecl{}});
  unit_lowerer.MapStructuralDataObjectBinding(
      loop_variable, lowerer.Frame(), variable);

  const auto lower =
      [&](const slang::ast::Expression& expr) -> diag::Result<hir::ExprId> {
    auto lowered = lowerer.LowerExpr(expr, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  };
  auto initial = lower(*array.initialExpression);
  if (!initial) return std::unexpected(std::move(initial.error()));
  auto condition = lower(*array.stopExpression);
  if (!condition) return std::unexpected(std::move(condition.error()));
  auto step = lower(*array.iterExpression);
  if (!step) return std::unexpected(std::move(step.error()));

  const hir::TypeId variable_type =
      frame.current_structural_scope->structural_data_objects.Get(variable)
          .type;
  auto index_ref = unit_lowerer.MakeRoutedValueRef(
      loop_variable, lowerer.Frame(), ScopeRoute::Enclosing({}));
  if (!index_ref) return std::unexpected(std::move(index_ref.error()));
  const hir::ExprId index_read =
      frame.Exprs().Add(hir::MakeRefExpr(*index_ref, variable_type, span));

  return hir::BlocksRepeat{
      .variable = variable,
      .initial = *initial,
      .condition = *condition,
      .step = *step,
      .index = index_read};
}

}  // namespace

auto StructuralScopeLowerer::BuildGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<BuiltGenerate> {
  if (!LoopSurvivedElaboration(array)) {
    throw InternalError(
        std::format(
            "StructuralScopeLowerer: the loop generate '{}' reached lowering "
            "without what a construction runs -- please report this as a bug",
            array.getExternalName()));
  }
  auto loop = BuildTheLoop(*this, array, frame);
  if (!loop) return std::unexpected(std::move(loop.error()));

  // Each block the loop counted out is lowered from its own elaboration, in
  // the order the loop counts them (LRM 27.4); which body each is, is this
  // object's, and the index reaches each as a value its construction supplies.
  BuiltGenerate built{.generate = {}, .blocks = {}};
  built.blocks.reserve(array.entries.size());
  for (const slang::ast::GenerateBlockSymbol* entry : array.entries) {
    auto block = LowerGenerateBlock(*owner_, *entry, array.name, frame);
    if (!block) return std::unexpected(std::move(block.error()));
    built.blocks.push_back(*std::move(block));
  }
  built.generate.counting = *std::move(loop);
  return built;
}

auto StructuralScopeLowerer::BuildGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<BuiltGenerate> {
  // A block no conditional produced stands on its own and is built at its one
  // place.
  if (!IsAlternative(block)) {
    auto built = LowerGenerateBlock(*owner_, block, block.name, frame);
    if (!built) return std::unexpected(std::move(built.error()));
    return BuiltGenerate{.generate = {}, .blocks = {*std::move(built)}};
  }

  // What selects an alternative is the source's and is stated wherever the
  // construct stands, so every alternative is placed here whether or not this
  // object selected it (LRM 27.5). Only the one it selected was elaborated, and
  // an alternative it did not select need not be valid for its values, so
  // that one alone is lowered, at its place.
  BuiltGenerate built{.generate = {}, .blocks = {}};
  SelectionTree selection;
  std::uint32_t position = 0;
  for (const auto* arm : AlternativesOfConstruct(block)) {
    auto placed = selection.Place(*this, frame, arm->selectionPath, position);
    if (!placed) return std::unexpected(std::move(placed.error()));
    ++position;
    if (arm->isUninstantiated) {
      built.blocks.push_back(hir::BlockConstruction{.body = {}, .built = {}});
      continue;
    }
    // The front end spells the name LRM 27.6 gives an unnamed alternative only
    // on a block it built.
    auto lowered = LowerGenerateBlock(*owner_, *arm, arm->name, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    built.blocks.push_back(*std::move(lowered));
  }
  built.generate.counting = selection.Compose(position);
  return built;
}

}  // namespace lyra::lowering::ast_to_hir
