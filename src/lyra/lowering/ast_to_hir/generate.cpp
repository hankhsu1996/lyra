#include <algorithm>
#include <array>
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
#include "lyra/lowering/ast_to_hir/one_body.hpp"
#include "lyra/lowering/ast_to_hir/selection_tree.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lowers one generate block -- a loop iteration, an `if` / `case` arm, or a
// bare block -- into a fresh concrete structural scope. The source label is
// settled here, on the scope itself; the hierarchy index is not, because a
// loop's blocks differ in it by definition and one scope serves every block
// its loop builds of it.
auto LowerGenerateScope(
    UnitLowerer& unit_lowerer, const slang::ast::GenerateBlockSymbol& block,
    std::string_view source_name, WalkFrame frame)
    -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer child(unit_lowerer, block);
  auto scope_or = child.Run(frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  scope_or->source_name = NameInAPath(source_name, block.getCompilation());
  return scope_or;
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
// step writing it -- and the index as each construction passes it to the body.
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
      .arguments = {index_read},
      .bodies = {}};
}

}  // namespace

auto StructuralScopeLowerer::BuildGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  if (!LoopSurvivedElaboration(array)) {
    throw InternalError(
        std::format(
            "StructuralScopeLowerer: the loop generate '{}' reached lowering "
            "without what a construction runs -- please report this as a bug",
            array.getExternalName()));
  }
  auto loop = BuildTheLoop(*this, array, frame);
  if (!loop) return std::unexpected(std::move(loop.error()));

  // Each block is lowered and taken into the body of its name; the index
  // reaches it as a value its construction supplies.
  LoopBodies bodies;
  for (const auto* entry : array.entries) {
    auto scope_or = LowerGenerateScope(*owner_, *entry, array.name, frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    const std::array index{LoopIndexOf(*entry)};
    if (!bodies.Take(
            *std::move(scope_or),
            owner_->Specialization().CodeArgumentsOf(*entry), index)) {
      throw InternalError(
          std::format(
              "StructuralScopeLowerer: the block of loop generate '{}' at {} "
              "lowers to something the blocks of its name before it do not "
              "-- please report this as a bug",
              array.getExternalName(), index[0]));
    }
  }
  return std::move(bodies).Build(*std::move(loop));
}

auto StructuralScopeLowerer::BuildGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  // A block no conditional produced stands on its own, and there is exactly
  // one of it.
  if (!IsAlternative(block)) {
    hir::Generate gen{};
    auto scope_or = LowerGenerateScope(*owner_, block, block.name, frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    gen.blocks.Add(hir::GenerateBlock{.scope = *std::move(scope_or)});
    return gen;
  }

  // What selects an alternative is the source's and is stated wherever the
  // construct stands, so every alternative is lowered here whether or not this
  // elaboration selected it (LRM 27.5); a body is lowered only for the one
  // that was, because an unselected block is not part of the model and its own
  // names need not resolve.
  hir::Generate gen{};
  std::vector<std::optional<hir::StructuralScopeId>> alternatives;
  SelectionTree selection;
  for (const auto* arm : AlternativesOfConstruct(block)) {
    const auto position = static_cast<std::uint32_t>(alternatives.size());
    auto placed = selection.Place(*this, frame, arm->selectionPath, position);
    if (!placed) return std::unexpected(std::move(placed.error()));
    std::optional<hir::StructuralScopeId> body;
    if (!arm->isUninstantiated) {
      auto scope_or = LowerGenerateScope(*owner_, *arm, arm->name, frame);
      if (!scope_or) return std::unexpected(std::move(scope_or.error()));
      body = gen.blocks.Add(hir::GenerateBlock{.scope = *std::move(scope_or)});
    }
    alternatives.push_back(body);
  }
  gen.counting = selection.Compose(std::move(alternatives));
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
