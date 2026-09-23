#include <algorithm>
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
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lowers one generate block -- a loop iteration, an `if` / `case` arm, or a
// bare block -- into a fresh concrete structural scope. The source label is
// settled here, on the scope itself; the hierarchy index is not, because a
// loop's blocks differ in it by definition and the scopes are compared with
// each other before anything knows which of them survives.
// `construction_value`, when present, is the block's own index, declared so
// that a name reaching it reads what construction supplied instead of folding
// to one block's value.
auto LowerGenerateScope(
    UnitLowerer& unit_lowerer, const slang::ast::GenerateBlockSymbol& block,
    std::string_view source_name, WalkFrame frame,
    StructuralScopeLowerer::ConstructionValue* construction_value = nullptr)
    -> diag::Result<hir::StructuralScope> {
  StructuralScopeLowerer child(unit_lowerer, block);
  auto scope_or = child.Run(frame, construction_value);
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

// Whether the loop the source wrote survived elaboration as something a
// construction can run: it counts out at least one block, it is named, and it
// has the three expressions that carry the index from one block to the next.
// How the step is written is not among the conditions, because every form LRM
// 27.4 admits for one states where the index goes next and is carried down as
// written.
auto LoopSurvivedElaboration(const slang::ast::GenerateBlockArraySymbol& array)
    -> bool {
  return array.valid && !array.entries.empty() && !array.name.empty() &&
         array.loopVariable != nullptr && array.initialExpression != nullptr &&
         array.stopExpression != nullptr && array.iterExpression != nullptr;
}

// The loop itself, for a generate whose blocks turned out to be one body: the
// index declared by the scope holding the generate, and the three expressions
// that reach it the way any name reaches a declaration -- the condition reading
// it, the step writing it.
auto BuildTheLoop(
    StructuralScopeLowerer& lowerer,
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame,
    hir::StructuralDataObjectId body_index) -> diag::Result<hir::BlocksRepeat> {
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
      loop_variable, lowerer.Frame(), variable, *index_type);

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
      .step = *step,
      .index = body_index};
}

}  // namespace

auto StructuralScopeLowerer::BuildGenerateFromArray(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  // Every block is lowered, and the index reaches each one as a value its
  // construction supplies rather than as a constant folded into it. That is
  // what makes one body possible at all, and it is also what makes the blocks
  // comparable: two that differ in nothing else then lower to the same scope.
  //
  // Whether one scope serves them all is what the lowered scopes say, and the
  // comparison is derived from the node definitions rather than written, so a
  // field added anywhere below is compared without anyone remembering to.
  std::vector<hir::StructuralScope> blocks;
  blocks.reserve(array.entries.size());
  std::optional<hir::StructuralDataObjectId> first_index;
  for (const auto* entry : array.entries) {
    ConstructionValue index{.parameter = BlockIndexParameter(*entry)};
    auto scope_or = LowerGenerateScope(
        *owner_, *entry, array.name, frame,
        index.parameter == nullptr ? nullptr : &index);
    if (!scope_or) return std::unexpected(std::move(scope_or.error()));
    if (blocks.empty() && index.parameter != nullptr) {
      first_index = index.declared;
    }
    blocks.push_back(*std::move(scope_or));
  }

  const bool one_body =
      LoopSurvivedElaboration(array) && first_index.has_value() &&
      std::ranges::all_of(blocks, [&](const hir::StructuralScope& block) {
        return block == blocks.front();
      });

  hir::Generate gen{};
  if (one_body) {
    auto counting = BuildTheLoop(*this, array, frame, *first_index);
    if (!counting) return std::unexpected(std::move(counting.error()));
    gen.child_scopes.Add(std::move(blocks.front()));
    gen.counting = *std::move(counting);
    return gen;
  }

  // Blocks that disagree are children in their own right, and the hierarchy
  // index each was elaborated at is what tells them apart -- which is why it
  // takes no part in the comparison above and is stamped on only once the
  // comparison is over.
  for (std::size_t at = 0; at < blocks.size(); ++at) {
    const slang::SVInt* array_index = array.entries[at]->getArrayIndex();
    if (array_index == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::BuildGenerateFromArray: loop iteration "
          "entry carries no array index");
    }
    blocks[at].index = array_index->as<std::int64_t>().value_or(0);
    gen.child_scopes.Add(std::move(blocks[at]));
  }
  return gen;
}

auto StructuralScopeLowerer::BuildGenerateFromBlock(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  hir::Generate gen{};
  auto scope_or = LowerGenerateScope(*owner_, block, block.name, frame);
  if (!scope_or) return std::unexpected(std::move(scope_or.error()));
  gen.child_scopes.Add(*std::move(scope_or));
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
