#include "generate.hpp"

#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>

#include "expression/lower.hpp"
#include "facts.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto AddGenerateChildScope(hir::Generate& gen, hir::StructuralScope scope)
    -> hir::StructuralScopeId {
  const hir::StructuralScopeId id{
      static_cast<std::uint32_t>(gen.child_scopes.size())};
  scope.id = id;
  gen.child_scopes.push_back(std::move(scope));
  return id;
}

auto AddChildScope(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeStack& stack, hir::Generate& generate,
    const slang::ast::GenerateBlockSymbol& block)
    -> diag::Result<hir::StructuralScopeId> {
  hir::StructuralScope scope;
  auto r = LowerScopeInto(unit_facts, unit_state, scope, block, stack);
  if (!r) return std::unexpected(std::move(r.error()));
  return AddGenerateChildScope(generate, std::move(scope));
}

}  // namespace

auto BuildIfGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> diag::Result<hir::Generate> {
  const slang::ast::GenerateBlockSymbol* then_block = nullptr;
  const slang::ast::GenerateBlockSymbol* else_block = nullptr;
  for (const auto* block : siblings) {
    switch (block->branchKind) {
      case slang::ast::GenerateBranchKind::IfTrue:
        then_block = block;
        break;
      case slang::ast::GenerateBranchKind::IfFalse:
        else_block = block;
        break;
      default:
        throw InternalError(
            "BuildIfGenerate: unexpected branch kind in if-generate sibling "
            "group");
    }
  }
  if (then_block == nullptr) {
    throw InternalError(
        "BuildIfGenerate: if-generate group has no IfTrue branch");
  }
  const auto* cond = then_block->conditionExpression;
  if (cond == nullptr) {
    throw InternalError(
        "BuildIfGenerate: IfTrue branch has no bound condition expression");
  }
  if (else_block != nullptr && else_block->conditionExpression != cond) {
    throw InternalError(
        "BuildIfGenerate: sibling branches have mismatched condition "
        "expressions");
  }

  auto cond_expr = LowerStructuralExpr(unit_facts, *cond);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = parent_state.AddExpr(*std::move(cond_expr));

  hir::Generate gen{};

  auto then_id = AddChildScope(unit_facts, unit_state, stack, gen, *then_block);
  if (!then_id) return std::unexpected(std::move(then_id.error()));

  std::optional<hir::StructuralScopeId> else_id;
  if (else_block != nullptr) {
    auto built = AddChildScope(unit_facts, unit_state, stack, gen, *else_block);
    if (!built) return std::unexpected(std::move(built.error()));
    else_id = *built;
  }

  gen.data = hir::IfGenerate{
      .condition = cond_id, .then_scope = *then_id, .else_scope = else_id};
  return gen;
}

auto BuildCaseGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> diag::Result<hir::Generate> {
  if (siblings.empty()) {
    throw InternalError(
        "BuildCaseGenerate: case-generate sibling group is empty");
  }
  const auto* discriminator = siblings.front()->conditionExpression;
  if (discriminator == nullptr) {
    throw InternalError(
        "BuildCaseGenerate: sibling has no condition expression");
  }
  for (const auto* block : siblings) {
    if (block->conditionExpression != discriminator) {
      throw InternalError(
          "BuildCaseGenerate: siblings have mismatched condition "
          "expressions");
    }
  }

  auto cond_expr = LowerStructuralExpr(unit_facts, *discriminator);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = parent_state.AddExpr(*std::move(cond_expr));

  hir::Generate gen{};

  std::vector<hir::CaseGenerateItem> items;
  std::optional<hir::StructuralScopeId> default_id;

  for (const auto* block : siblings) {
    switch (block->branchKind) {
      case slang::ast::GenerateBranchKind::CaseItem: {
        std::vector<hir::ExprId> labels;
        labels.reserve(block->caseItemExpressions.size());
        for (const auto* label_expr : block->caseItemExpressions) {
          auto label_expr_lowered =
              LowerStructuralExpr(unit_facts, *label_expr);
          if (!label_expr_lowered)
            return std::unexpected(std::move(label_expr_lowered.error()));
          labels.push_back(
              parent_state.AddExpr(*std::move(label_expr_lowered)));
        }
        auto item_id =
            AddChildScope(unit_facts, unit_state, stack, gen, *block);
        if (!item_id) return std::unexpected(std::move(item_id.error()));
        items.push_back(
            hir::CaseGenerateItem{
                .labels = std::move(labels), .scope = *item_id});
        break;
      }
      case slang::ast::GenerateBranchKind::CaseDefault: {
        if (default_id.has_value()) {
          throw InternalError(
              "BuildCaseGenerate: case-generate has more than one default "
              "branch");
        }
        auto built = AddChildScope(unit_facts, unit_state, stack, gen, *block);
        if (!built) return std::unexpected(std::move(built.error()));
        default_id = *built;
        break;
      }
      default:
        throw InternalError(
            "BuildCaseGenerate: unexpected branch kind in case-generate");
    }
  }

  gen.data = hir::CaseGenerate{
      .condition = cond_id,
      .items = std::move(items),
      .default_scope = default_id};
  return gen;
}

auto BuildLoopGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    const slang::ast::GenerateBlockArraySymbol& array)
    -> diag::Result<hir::Generate> {
  if (array.genvar == nullptr || array.initialExpression == nullptr ||
      array.stopExpression == nullptr || array.iterExpression == nullptr) {
    throw InternalError(
        "BuildLoopGenerate: GenerateBlockArraySymbol is missing bound "
        "header expressions or canonical genvar");
  }

  LoopHeaderState loop_state{
      .expected_name = array.genvar->name,
      .synthetic_symbol = nullptr,
      .loop_var_id = std::nullopt};

  auto initial_expr = LowerLoopHeaderExpr(
      unit_facts, unit_state, parent_state, stack, loop_state,
      *array.initialExpression);
  if (!initial_expr) return std::unexpected(std::move(initial_expr.error()));
  const hir::ExprId initial_id = parent_state.AddExpr(*std::move(initial_expr));

  auto stop_expr = LowerLoopHeaderExpr(
      unit_facts, unit_state, parent_state, stack, loop_state,
      *array.stopExpression);
  if (!stop_expr) return std::unexpected(std::move(stop_expr.error()));
  const hir::ExprId stop_id = parent_state.AddExpr(*std::move(stop_expr));

  auto iter_expr = LowerLoopHeaderExpr(
      unit_facts, unit_state, parent_state, stack, loop_state,
      *array.iterExpression);
  if (!iter_expr) return std::unexpected(std::move(iter_expr.error()));
  const hir::ExprId iter_id = parent_state.AddExpr(*std::move(iter_expr));

  if (!loop_state.loop_var_id.has_value()) {
    throw InternalError(
        "BuildLoopGenerate: loop-generate header did not expose a "
        "synthetic loop variable");
  }

  hir::Generate gen{};
  const hir::StructuralScopeId body_scope_id =
      AddGenerateChildScope(gen, hir::StructuralScope{});

  gen.data = hir::LoopGenerate{
      .loop_var = *loop_state.loop_var_id,
      .initial = initial_id,
      .stop = stop_id,
      .iter = iter_id,
      .body_scope = body_scope_id};
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
