#include "generate.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "expression/lower.hpp"
#include "facts.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/support/internal_error.hpp"
#include "scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto AddChildScope(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeStack& stack, std::vector<hir::StructuralScope>& child_scopes,
    const slang::ast::GenerateBlockSymbol& block)
    -> diag::Result<hir::StructuralScopeId> {
  hir::StructuralScope scope;
  auto r = LowerScopeInto(unit_facts, unit_state, scope, block, stack);
  if (!r) return std::unexpected(std::move(r.error()));
  const hir::StructuralScopeId id{
      .value = static_cast<std::uint32_t>(child_scopes.size())};
  child_scopes.push_back(std::move(scope));
  return id;
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
        throw support::InternalError(
            "BuildIfGenerate: unexpected branch kind in if-generate sibling "
            "group");
    }
  }
  if (then_block == nullptr) {
    throw support::InternalError(
        "BuildIfGenerate: if-generate group has no IfTrue branch");
  }
  const auto* cond = then_block->conditionExpression;
  if (cond == nullptr) {
    throw support::InternalError(
        "BuildIfGenerate: IfTrue branch has no bound condition expression");
  }
  if (else_block != nullptr && else_block->conditionExpression != cond) {
    throw support::InternalError(
        "BuildIfGenerate: sibling branches have mismatched condition "
        "expressions");
  }

  auto cond_data =
      LowerExpressionData(unit_facts, parent_state.UnitState(), stack, *cond);
  if (!cond_data) return std::unexpected(std::move(cond_data.error()));
  const hir::ExprId cond_id = parent_state.AppendExpr(*std::move(cond_data));

  std::vector<hir::StructuralScope> child_scopes;
  child_scopes.reserve(else_block != nullptr ? 2 : 1);

  auto then_id =
      AddChildScope(unit_facts, unit_state, stack, child_scopes, *then_block);
  if (!then_id) return std::unexpected(std::move(then_id.error()));

  std::optional<hir::StructuralScopeId> else_id;
  if (else_block != nullptr) {
    auto built =
        AddChildScope(unit_facts, unit_state, stack, child_scopes, *else_block);
    if (!built) return std::unexpected(std::move(built.error()));
    else_id = *built;
  }

  return hir::Generate{
      .data =
          hir::IfGenerate{
              .condition = cond_id,
              .then_scope = *then_id,
              .else_scope = else_id},
      .child_scopes = std::move(child_scopes)};
}

auto BuildCaseGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> diag::Result<hir::Generate> {
  if (siblings.empty()) {
    throw support::InternalError(
        "BuildCaseGenerate: case-generate sibling group is empty");
  }
  const auto* discriminator = siblings.front()->conditionExpression;
  if (discriminator == nullptr) {
    throw support::InternalError(
        "BuildCaseGenerate: sibling has no condition expression");
  }
  for (const auto* block : siblings) {
    if (block->conditionExpression != discriminator) {
      throw support::InternalError(
          "BuildCaseGenerate: siblings have mismatched condition "
          "expressions");
    }
  }

  auto cond_data = LowerExpressionData(
      unit_facts, parent_state.UnitState(), stack, *discriminator);
  if (!cond_data) return std::unexpected(std::move(cond_data.error()));
  const hir::ExprId cond_id = parent_state.AppendExpr(*std::move(cond_data));

  std::vector<hir::StructuralScope> child_scopes;
  child_scopes.reserve(siblings.size());

  std::vector<hir::CaseGenerateItem> items;
  std::optional<hir::StructuralScopeId> default_id;

  for (const auto* block : siblings) {
    switch (block->branchKind) {
      case slang::ast::GenerateBranchKind::CaseItem: {
        std::vector<hir::ExprId> labels;
        labels.reserve(block->caseItemExpressions.size());
        for (const auto* label_expr : block->caseItemExpressions) {
          auto label_data = LowerExpressionData(
              unit_facts, parent_state.UnitState(), stack, *label_expr);
          if (!label_data)
            return std::unexpected(std::move(label_data.error()));
          labels.push_back(parent_state.AppendExpr(*std::move(label_data)));
        }
        auto item_id =
            AddChildScope(unit_facts, unit_state, stack, child_scopes, *block);
        if (!item_id) return std::unexpected(std::move(item_id.error()));
        items.push_back(
            hir::CaseGenerateItem{
                .labels = std::move(labels), .scope = *item_id});
        break;
      }
      case slang::ast::GenerateBranchKind::CaseDefault: {
        if (default_id.has_value()) {
          throw support::InternalError(
              "BuildCaseGenerate: case-generate has more than one default "
              "branch");
        }
        auto built =
            AddChildScope(unit_facts, unit_state, stack, child_scopes, *block);
        if (!built) return std::unexpected(std::move(built.error()));
        default_id = *built;
        break;
      }
      default:
        throw support::InternalError(
            "BuildCaseGenerate: unexpected branch kind in case-generate");
    }
  }

  return hir::Generate{
      .data =
          hir::CaseGenerate{
              .condition = cond_id,
              .items = std::move(items),
              .default_scope = default_id},
      .child_scopes = std::move(child_scopes)};
}

}  // namespace lyra::lowering::ast_to_hir
