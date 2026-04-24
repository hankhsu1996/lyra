#include "generate.hpp"

#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "expression/lower.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/support/unsupported.hpp"
#include "scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto AddChildScope(
    UnitLoweringState& unit, ScopeStack& stack,
    std::vector<hir::StructuralScope>& child_scopes,
    const slang::ast::GenerateBlockSymbol& block) -> hir::StructuralScopeId {
  hir::StructuralScope scope;
  LowerScope(unit, scope, block, stack);
  const hir::StructuralScopeId id{
      .value = static_cast<std::uint32_t>(child_scopes.size())};
  child_scopes.push_back(std::move(scope));
  return id;
}

}  // namespace

auto BuildIfGenerate(
    UnitLoweringState& unit, ScopeLoweringState& parent_state,
    ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> hir::Generate {
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
        support::Unsupported(
            "BuildIfGenerate: unexpected branchKind in if-generate sibling");
    }
  }

  if (then_block == nullptr) {
    support::Unsupported(
        "BuildIfGenerate: if-generate sibling group missing IfTrue branch");
  }

  const auto* cond = then_block->conditionExpression;
  if (cond == nullptr) {
    support::Unsupported(
        "BuildIfGenerate: IfTrue without bound conditionExpression");
  }
  if (else_block != nullptr && else_block->conditionExpression != cond) {
    support::Unsupported(
        "BuildIfGenerate: sibling branches have mismatched "
        "conditionExpression");
  }

  const hir::ExprId cond_id =
      LowerStructuralExpression(parent_state, stack, *cond);

  std::vector<hir::StructuralScope> child_scopes;
  child_scopes.reserve(else_block != nullptr ? 2 : 1);

  const hir::StructuralScopeId then_id =
      AddChildScope(unit, stack, child_scopes, *then_block);

  std::optional<hir::StructuralScopeId> else_id;
  if (else_block != nullptr) {
    else_id = AddChildScope(unit, stack, child_scopes, *else_block);
  }

  return hir::Generate{
      .data =
          hir::IfGenerate{
              .condition = cond_id,
              .then_scope = then_id,
              .else_scope = else_id},
      .child_scopes = std::move(child_scopes)};
}

auto BuildCaseGenerate(
    UnitLoweringState& unit, ScopeLoweringState& parent_state,
    ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> hir::Generate {
  if (siblings.empty()) {
    support::Unsupported("BuildCaseGenerate: empty sibling group");
  }

  const auto* discriminator = siblings.front()->conditionExpression;
  if (discriminator == nullptr) {
    support::Unsupported(
        "BuildCaseGenerate: sibling missing conditionExpression");
  }
  for (const auto* block : siblings) {
    if (block->conditionExpression != discriminator) {
      support::Unsupported(
          "BuildCaseGenerate: sibling branches have mismatched "
          "conditionExpression");
    }
  }

  const hir::ExprId cond_id =
      LowerStructuralExpression(parent_state, stack, *discriminator);

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
          labels.push_back(
              LowerStructuralExpression(parent_state, stack, *label_expr));
        }
        const hir::StructuralScopeId item_id =
            AddChildScope(unit, stack, child_scopes, *block);
        items.push_back(
            hir::CaseGenerateItem{
                .labels = std::move(labels), .scope = item_id});
        break;
      }
      case slang::ast::GenerateBranchKind::CaseDefault: {
        if (default_id.has_value()) {
          support::Unsupported("BuildCaseGenerate: multiple default branches");
        }
        default_id = AddChildScope(unit, stack, child_scopes, *block);
        break;
      }
      default:
        support::Unsupported(
            "BuildCaseGenerate: unexpected branchKind in case construct");
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
