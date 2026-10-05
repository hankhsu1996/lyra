#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/one_body.hpp"
#include "lyra/lowering/ast_to_hir/selection_tree.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lowers one generate block -- a loop iteration, an `if` / `case` arm, or a
// bare block -- into a fresh concrete structural scope. The source label is
// settled here, on the scope itself; the hierarchy index is not, because a
// loop's blocks differ in it by definition and the scopes are compared with
// each other before anything knows which of them survives.
auto LowerGenerateScope(
    UnitLowerer& unit_lowerer, const slang::ast::GenerateBlockSymbol& block,
    std::string_view source_name, WalkFrame frame)
    -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer child(unit_lowerer, block);
  auto scope_or = child.Run(frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  scope_or->source_name = std::string{source_name};
  return scope_or;
}

// The implicit localparam the index name denotes inside one block (LRM 27.4).
// Every block declares its own, holding the value the index had when that block
// elaborated. The clause gives it the genvar's name, and the front end marks
// which declaration it is; the mark is what this asks, because a block's other
// parameters share the loop's name with nothing and must not be taken for it.
auto BlockIndexParameter(const slang::ast::GenerateBlockSymbol& block)
    -> const slang::ast::ParameterSymbol* {
  for (const auto& member : block.members()) {
    const auto* parameter = member.as_if<slang::ast::ParameterSymbol>();
    if (parameter != nullptr && parameter->isFromGenvar()) {
      return parameter;
    }
  }
  return nullptr;
}

// The values the loop's index stood at, one per block it counted out, in the
// order it counted them.
auto IndexValuesOf(const slang::ast::GenerateBlockArraySymbol& array)
    -> std::vector<std::int64_t> {
  std::vector<std::int64_t> values;
  values.reserve(array.entries.size());
  for (const slang::ast::GenerateBlockSymbol* entry : array.entries) {
    values.push_back(LoopIndexOf(*entry));
  }
  return values;
}

// Whether the loop the source wrote survived elaboration as something a
// construction can run: it counts out at least one block, the block declares
// the index it is built with, it is named, and it has the three expressions
// that carry the index from one block to the next. How the step is written is
// not among the conditions, because every form LRM 27.4 admits for one states
// where the index goes next and is carried down as written.
auto LoopSurvivedElaboration(const slang::ast::GenerateBlockArraySymbol& array)
    -> bool {
  return array.valid && !array.entries.empty() &&
         BlockIndexParameter(*array.entries.front()) != nullptr &&
         !array.name.empty() && array.loopVariable != nullptr &&
         array.initialExpression != nullptr &&
         array.stopExpression != nullptr && array.iterExpression != nullptr;
}

// The loop itself, for a generate whose blocks turned out to be one body: the
// index declared by the scope holding the generate, and the three expressions
// that reach it the way any name reaches a declaration -- the condition reading
// it, the step writing it.
auto BuildTheLoop(
    StructuralScopeLowerer& lowerer,
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::BlocksRepeat> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const slang::ast::ValueSymbol& loop_variable = *array.loopVariable;
  const auto span =
      unit_lowerer.SourceMapper().PointSpanOf(loop_variable.location);

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

  return hir::BlocksRepeat{
      .variable = variable,
      .initial = *initial,
      .condition = *condition,
      .step = *step};
}

// A loop whose blocks are one body: that body is built at every index the loop
// counts out, and each time it receives the loop's index as it then stands. The
// variable is the holding scope's own, so the read reaches it over the route
// that climbs no edges.
auto BuildRepeatedGenerate(
    StructuralScopeLowerer& lowerer,
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame,
    hir::StructuralScope one_body) -> diag::Result<hir::Generate> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto counting = BuildTheLoop(lowerer, array, frame);
  if (!counting) return std::unexpected(std::move(counting.error()));

  const hir::StructuralDataObjectDecl& variable =
      frame.current_structural_scope->structural_data_objects.Get(
          counting->variable);
  auto index_ref = unit_lowerer.MakeRoutedValueRef(
      *array.loopVariable, lowerer.Frame(), ScopeRoute::Enclosing({}));
  if (!index_ref) return std::unexpected(std::move(index_ref.error()));
  const hir::ExprId index_read = frame.Exprs().Add(
      hir::MakeRefExpr(
          *index_ref, variable.type,
          unit_lowerer.SourceMapper().PointSpanOf(
              array.loopVariable->location)));

  hir::Generate gen{};
  gen.blocks.Add(
      hir::GenerateBlock{
          .scope = std::move(one_body), .arguments = {index_read}});
  gen.counting = *std::move(counting);
  return gen;
}

// A loop whose blocks disagree: each is a child in its own right, and the
// hierarchy index it was elaborated at is what tells it from the others. Each
// receives the value its own index holds.
auto BuildStandAloneGenerate(
    UnitLowerer& unit_lowerer,
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame,
    std::vector<hir::StructuralScope> blocks) -> diag::Result<hir::Generate> {
  hir::Generate gen{
      .blocks = {},
      .counting = hir::BlocksStandAlone{.indices = IndexValuesOf(array)}};
  for (std::size_t at = 0; at < blocks.size(); ++at) {
    const slang::ast::GenerateBlockSymbol& entry = *array.entries[at];
    std::vector<hir::ExprId> arguments;
    if (const slang::ast::ParameterSymbol* index = BlockIndexParameter(entry)) {
      const auto span =
          unit_lowerer.SourceMapper().PointSpanOf(index->location);
      auto type = unit_lowerer.InternType(index->getType(), span);
      if (!type) return std::unexpected(std::move(type.error()));
      auto value = MakeConstantValueExpr(
          unit_lowerer.Unit(), frame, index->getValue(), *type, span);
      if (!value) return std::unexpected(std::move(value.error()));
      arguments.push_back(frame.Exprs().Add(*std::move(value)));
    }
    gen.blocks.Add(
        hir::GenerateBlock{
            .scope = std::move(blocks[at]), .arguments = std::move(arguments)});
  }
  return gen;
}

}  // namespace

auto StructuralScopeLowerer::BuildGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  // Every block is lowered, and the index reaches each one as a value its
  // construction supplies rather than as a constant folded into it. That is
  // what makes one body possible at all, and it is also what makes the blocks
  // comparable: two that differ in nothing else then lower to the same scope,
  // and whether one scope serves them all is what the lowered scopes say.
  std::vector<hir::StructuralScope> blocks;
  blocks.reserve(array.entries.size());
  for (const auto* entry : array.entries) {
    auto scope_or = LowerGenerateScope(*owner_, *entry, array.name, frame);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    blocks.push_back(*std::move(scope_or));
  }

  if (LoopSurvivedElaboration(array)) {
    if (auto one_body = OneBodyOf(blocks)) {
      return BuildRepeatedGenerate(*this, array, frame, *std::move(one_body));
    }
  }
  return BuildStandAloneGenerate(*owner_, array, frame, std::move(blocks));
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
    gen.blocks.Add(
        hir::GenerateBlock{.scope = *std::move(scope_or), .arguments = {}});
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
      body = gen.blocks.Add(
          hir::GenerateBlock{.scope = *std::move(scope_or), .arguments = {}});
    }
    alternatives.push_back(body);
  }
  gen.counting = selection.Compose(std::move(alternatives));
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
