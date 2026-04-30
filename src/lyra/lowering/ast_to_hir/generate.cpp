#include "lyra/lowering/ast_to_hir/generate.hpp"

#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/expression/lower.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/scope.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto AddChildStructuralScope(hir::Generate& gen, hir::StructuralScope scope)
    -> hir::StructuralScopeId {
  const hir::StructuralScopeId id{
      static_cast<std::uint32_t>(gen.child_scopes.size())};
  scope.id = id;
  gen.child_scopes.push_back(std::move(scope));
  return id;
}

// Derive the per-entry implicit-genvar ParameterSymbol that substitutes for
// `array.loopVariable` inside the canonical entry body.
//
// Invariant: header refs to the loop variable use pointer identity to
// `array.loopVariable`; body refs use this derived ParameterSymbol. Both are
// registered as keys in UnitLoweringState's loop-var binding map and resolve
// to the same hir::LoopVarDeclId.
//
// The derivation is necessary because slang gives no API connecting the
// header `loopVariable` and an entry's implicit genvar parameter -- they
// share name, source location, and the `isFromGenvar` flag, but are
// independently allocated objects. See slang
// source/ast/symbols/BlockSymbols.cpp:794-816 for the construction.
//
// Caller must guard array.entries.empty() before calling this helper.
auto DeriveLoopVariableSubstitution(
    const slang::ast::GenerateBlockArraySymbol& array,
    const slang::ast::GenerateBlockSymbol& entry)
    -> const slang::ast::ParameterSymbol* {
  if (array.loopVariable == nullptr) {
    throw InternalError("DeriveLoopVariableSubstitution: missing loopVariable");
  }
  if (entry.getParentScope() != &array) {
    throw InternalError(
        "DeriveLoopVariableSubstitution: entry is not a direct child of array");
  }

  const slang::ast::ParameterSymbol* found = nullptr;
  for (const auto& param : entry.membersOfType<slang::ast::ParameterSymbol>()) {
    if (!param.isFromGenvar()) continue;
    if (param.location != array.loopVariable->location) continue;
    if (param.name != array.loopVariable->name) continue;
    if (found != nullptr) {
      throw InternalError(
          "DeriveLoopVariableSubstitution: ambiguous loop-variable "
          "substitution");
    }
    found = &param;
  }
  if (found == nullptr) {
    throw InternalError(
        "DeriveLoopVariableSubstitution: missing loop-variable substitution");
  }
  return found;
}

auto AddChildScope(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeStack& stack, hir::Generate& generate,
    const slang::ast::GenerateBlockSymbol& block)
    -> diag::Result<hir::StructuralScopeId> {
  hir::StructuralScope scope;
  auto r = LowerScopeInto(unit_facts, unit_state, scope, block, stack);
  if (!r) return std::unexpected(std::move(r.error()));
  return AddChildStructuralScope(generate, std::move(scope));
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
  const auto* cond = then_block->getConditionExpression();
  if (cond == nullptr) {
    throw InternalError(
        "BuildIfGenerate: IfTrue branch has no bound condition expression");
  }
  if (else_block != nullptr && else_block->getConditionExpression() != cond) {
    throw InternalError(
        "BuildIfGenerate: sibling branches have mismatched condition "
        "expressions");
  }

  auto cond_expr =
      LowerStructuralExpr(unit_facts, unit_state, parent_state, stack, *cond);
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
  const auto* discriminator = siblings.front()->getConditionExpression();
  if (discriminator == nullptr) {
    throw InternalError(
        "BuildCaseGenerate: sibling has no condition expression");
  }
  for (const auto* block : siblings) {
    if (block->getConditionExpression() != discriminator) {
      throw InternalError(
          "BuildCaseGenerate: siblings have mismatched condition "
          "expressions");
    }
  }

  auto cond_expr = LowerStructuralExpr(
      unit_facts, unit_state, parent_state, stack, *discriminator);
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
          auto label_expr_lowered = LowerStructuralExpr(
              unit_facts, unit_state, parent_state, stack, *label_expr);
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
  const auto* loop_var_sym = array.loopVariable;
  if (loop_var_sym == nullptr || array.initialExpression == nullptr ||
      array.stopExpression == nullptr || array.iterExpression == nullptr) {
    throw InternalError(
        "BuildLoopGenerate: GenerateBlockArraySymbol is missing loopVariable "
        "or bound header expressions");
  }

  const auto var_span =
      unit_facts.SourceMapper().PointSpanOf(loop_var_sym->location);
  auto type_data = LowerTypeData(loop_var_sym->getType(), var_span);
  if (!type_data) return std::unexpected(std::move(type_data.error()));
  const hir::TypeId loop_var_type = unit_state.AddType(*std::move(type_data));

  const hir::LoopVarDeclId loop_var_id =
      parent_state.AddLoopVarDecl(*loop_var_sym, loop_var_type);

  auto initial_expr = LowerStructuralExpr(
      unit_facts, unit_state, parent_state, stack, *array.initialExpression);
  if (!initial_expr) return std::unexpected(std::move(initial_expr.error()));
  const hir::ExprId initial_id = parent_state.AddExpr(*std::move(initial_expr));

  auto stop_expr = LowerStructuralExpr(
      unit_facts, unit_state, parent_state, stack, *array.stopExpression);
  if (!stop_expr) return std::unexpected(std::move(stop_expr.error()));
  const hir::ExprId stop_id = parent_state.AddExpr(*std::move(stop_expr));

  auto iter_expr = LowerStructuralExpr(
      unit_facts, unit_state, parent_state, stack, *array.iterExpression);
  if (!iter_expr) return std::unexpected(std::move(iter_expr.error()));
  const hir::ExprId iter_id = parent_state.AddExpr(*std::move(iter_expr));

  hir::Generate gen{};
  const hir::StructuralScopeId loop_scope_id =
      AddChildStructuralScope(gen, hir::StructuralScope{});

  if (!array.entries.empty()) {
    const auto& canonical_entry = *array.entries.front();
    const auto* body_param =
        DeriveLoopVariableSubstitution(array, canonical_entry);

    const ScopeEntryLoopVarBinding body_binding{
        .symbol = body_param,
        .home_frame = parent_state.Frame(),
        .loop_var = loop_var_id,
        .type = loop_var_type,
    };

    auto& loop_scope = gen.child_scopes.at(loop_scope_id.value);
    auto r = LowerScopeInto(
        unit_facts, unit_state, loop_scope, canonical_entry, stack,
        std::span{&body_binding, 1});
    if (!r) return std::unexpected(std::move(r.error()));
  }

  gen.data = hir::LoopGenerate{
      .loop_var = loop_var_id,
      .initial = initial_id,
      .stop = stop_id,
      .iter = iter_id,
      .scope = loop_scope_id};
  return gen;
}

}  // namespace lyra::lowering::ast_to_hir
