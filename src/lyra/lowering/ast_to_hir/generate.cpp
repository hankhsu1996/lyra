#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <map>
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
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The body the block instances `built` are, in the order the lowering reads
// them, lowered from the first. The source label is settled here, on the scope
// itself; the hierarchy index is not, because a loop's blocks differ in it by
// definition and one scope serves every block its loop builds of it. A name the
// body reaches outward resolves through the scopes holding the block it is
// lowered from, which need not be the elaboration the walk holding the
// construct is lowered from.
auto LowerGenerateBody(
    UnitLowerer& unit_lowerer, std::vector<const slang::ast::Scope*> built,
    std::string_view source_name, const WalkFrame& frame)
    -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer body(unit_lowerer, std::move(built));
  const slang::ast::Scope& holder =
      *body.LoweredFrom().asSymbol().getParentScope();
  auto scope_or = body.Run(frame.WithDeclaringScope(
      unit_lowerer.DeclaringScopeChain(holder), &holder));
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  scope_or->source_name =
      NameInAPath(source_name, body.LoweredFrom().getCompilation());
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

  // Every block any elaboration of this scope counted out, in the order the
  // lowering reads them, under the body it is: blocks of one class and one body
  // of it are one body wherever they stand, and the index reaches each as a
  // value its construction supplies (LRM 27.4). Which body stands at an index
  // is what the first elaboration read to count a block out there says.
  const SpecializationPolicy& policy = owner_->Specialization();
  using BodyName =
      std::pair<support::GenerateBlockStep, std::optional<std::uint64_t>>;
  std::map<BodyName, std::vector<const slang::ast::Scope*>> blocks_of;
  std::map<std::int64_t, BodyName> standing_at;
  for (const slang::ast::Scope* elaboration : elaborations_) {
    const auto& counted = CounterpartIn(*elaboration, array);
    for (const slang::ast::Scope* entry : owner_->InReadingOrder(
             {counted.entries.begin(), counted.entries.end()})) {
      const auto& block =
          entry->asSymbol().as<slang::ast::GenerateBlockSymbol>();
      BodyName name{policy.BlockStepOf(block), policy.BodyArgumentsOf(block)};
      standing_at.try_emplace(LoopIndexOf(block), name);
      blocks_of[std::move(name)].push_back(entry);
    }
  }

  // A body is built where it stands, in the order of the first index it stands
  // at. A loop of one body has nothing to look up and lists no values: it
  // builds that body at every index it counts out, however many that is.
  const bool several_bodies = blocks_of.size() > 1;
  hir::Generate gen{};
  std::map<BodyName, std::size_t> position_of;
  for (const auto& [index, name] : standing_at) {
    const auto [at, first] = position_of.try_emplace(name, loop->bodies.size());
    if (first) {
      auto scope_or =
          LowerGenerateBody(*owner_, blocks_of.at(name), array.name, frame);
      if (!scope_or) return std::unexpected(std::move(scope_or.error()));
      loop->bodies.push_back(
          hir::LoopBody{
              .body = gen.blocks.Add(
                  hir::GenerateBlock{.scope = *std::move(scope_or)}),
              .body_arguments = name.second,
              .indices = {}});
    }
    if (several_bodies) loop->bodies[at->second].indices.push_back(index);
  }
  gen.counting = *std::move(loop);
  return gen;
}

auto StructuralScopeLowerer::BuildGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  // A block no conditional produced stands on its own, and every elaboration
  // built it.
  if (!IsAlternative(block)) {
    std::vector<const slang::ast::Scope*> built;
    for (const slang::ast::Scope* elaboration : elaborations_) {
      built.push_back(&CounterpartIn(*elaboration, block));
    }
    hir::Generate gen{};
    auto scope_or =
        LowerGenerateBody(*owner_, std::move(built), block.name, frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    gen.blocks.Add(hir::GenerateBlock{.scope = *std::move(scope_or)});
    return gen;
  }

  // What selects an alternative is the source's and is stated wherever the
  // construct stands, so every alternative is placed here whether or not any
  // elaboration selected it (LRM 27.5). A body is lowered for each alternative
  // some elaboration of this scope selected, from those that did; one none
  // selected is not part of the model, and its own names need not resolve.
  std::vector<std::vector<const slang::ast::GenerateBlockSymbol*>>
      alternatives_by_elaboration;
  for (const slang::ast::Scope* elaboration : elaborations_) {
    alternatives_by_elaboration.push_back(
        AlternativesOfConstruct(CounterpartIn(*elaboration, block)));
  }
  hir::Generate gen{};
  std::vector<std::optional<hir::StructuralScopeId>> alternatives;
  SelectionTree selection;
  for (const auto* arm : alternatives_by_elaboration.front()) {
    const auto position = static_cast<std::uint32_t>(alternatives.size());
    auto placed = selection.Place(*this, frame, arm->selectionPath, position);
    if (!placed) return std::unexpected(std::move(placed.error()));
    std::vector<const slang::ast::Scope*> built;
    for (const auto& elaboration : alternatives_by_elaboration) {
      if (!elaboration[position]->isUninstantiated) {
        built.push_back(elaboration[position]);
      }
    }
    std::optional<hir::StructuralScopeId> body;
    if (!built.empty()) {
      // The front end spells the name LRM 27.6 gives an unnamed alternative
      // only on a block it built.
      const std::string_view name = built.front()->asSymbol().name;
      auto scope_or = LowerGenerateBody(*owner_, std::move(built), name, frame);
      if (!scope_or) return std::unexpected(std::move(scope_or.error()));
      body = gen.blocks.Add(hir::GenerateBlock{.scope = *std::move(scope_or)});
    }
    alternatives.push_back(body);
  }
  gen.counting = selection.Compose(std::move(alternatives));
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
