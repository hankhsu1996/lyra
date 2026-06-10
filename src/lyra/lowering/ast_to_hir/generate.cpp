#include <expected>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/scope_lowerer.hpp"

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
// `array.loopVariable` inside the canonical entry body. See
// slang source/ast/symbols/BlockSymbols.cpp:794-816 for construction details.
auto DeriveLoopVariableSubstitution(
    const slang::ast::GenerateBlockArraySymbol& array,
    const slang::ast::GenerateBlockSymbol& entry)
    -> const slang::ast::ParameterSymbol* {
  if (array.loopVariable == nullptr) {
    throw InternalError("DeriveLoopVariableSubstitution: missing loopVariable");
  }
  if (entry.getParentScope() != &array) {
    throw InternalError(
        "DeriveLoopVariableSubstitution: entry is not a direct child of "
        "array");
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

// Adds a generate child scope: constructs a fresh ScopeLowerer for the slang
// block, runs it, registers the owned-child binding.
auto AddChildScope(
    ModuleLowerer& module, hir::Generate& generate, ScopeFrameId home_frame,
    hir::GenerateId generate_id, const slang::ast::GenerateBlockSymbol& block,
    WalkFrame frame) -> diag::Result<hir::StructuralScopeId> {
  hir::StructuralScope scope;
  scope.source_name = std::string{block.name};

  // Insert the scope first so we can pass its inner reference to the child
  // lowerer; child writes go into the inserted scope.
  const hir::StructuralScopeId scope_id =
      AddChildStructuralScope(generate, std::move(scope));
  auto& inserted_scope = generate.child_scopes.at(scope_id.value);

  ScopeLowerer child(module, inserted_scope, block);
  module.MapOwnedChildBinding(
      block, home_frame,
      hir::DownwardHead{
          .child = hir::GenerateChildRef{
              .generate = generate_id, .scope = scope_id}});
  auto r = child.Run(frame);
  if (!r) return std::unexpected(std::move(r.error()));
  return scope_id;
}

}  // namespace

auto ScopeLowerer::BuildIfGenerate(
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings,
    WalkFrame frame) -> diag::Result<hir::Generate> {
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
            "ScopeLowerer::BuildIfGenerate: unexpected branch kind in "
            "if-generate sibling group");
    }
  }
  if (then_block == nullptr) {
    throw InternalError(
        "ScopeLowerer::BuildIfGenerate: if-generate group has no IfTrue "
        "branch");
  }
  const auto* cond = then_block->getConditionExpression();
  if (cond == nullptr) {
    throw InternalError(
        "ScopeLowerer::BuildIfGenerate: IfTrue branch has no bound "
        "condition expression");
  }
  if (else_block != nullptr && else_block->getConditionExpression() != cond) {
    throw InternalError(
        "ScopeLowerer::BuildIfGenerate: sibling branches have mismatched "
        "condition expressions");
  }

  auto cond_expr = LowerExpr(*cond, frame);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = AddExpr(*std::move(cond_expr));

  hir::Generate gen{};
  const ScopeFrameId gen_frame = frame_;
  const hir::GenerateId gen_id = NextGenerateId();

  auto then_id =
      AddChildScope(*module_, gen, gen_frame, gen_id, *then_block, frame);
  if (!then_id) return std::unexpected(std::move(then_id.error()));

  std::optional<hir::StructuralScopeId> else_id;
  if (else_block != nullptr) {
    auto built =
        AddChildScope(*module_, gen, gen_frame, gen_id, *else_block, frame);
    if (!built) return std::unexpected(std::move(built.error()));
    else_id = *built;
  }

  gen.data = hir::IfGenerate{
      .condition = cond_id, .then_scope = *then_id, .else_scope = else_id};
  return gen;
}

auto ScopeLowerer::BuildCaseGenerate(
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings,
    WalkFrame frame) -> diag::Result<hir::Generate> {
  if (siblings.empty()) {
    throw InternalError(
        "ScopeLowerer::BuildCaseGenerate: case-generate sibling group is "
        "empty");
  }
  const auto* discriminator = siblings.front()->getConditionExpression();
  if (discriminator == nullptr) {
    throw InternalError(
        "ScopeLowerer::BuildCaseGenerate: sibling has no condition expression");
  }
  for (const auto* block : siblings) {
    if (block->getConditionExpression() != discriminator) {
      throw InternalError(
          "ScopeLowerer::BuildCaseGenerate: siblings have mismatched "
          "condition expressions");
    }
  }

  auto cond_expr = LowerExpr(*discriminator, frame);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = AddExpr(*std::move(cond_expr));

  hir::Generate gen{};
  const ScopeFrameId gen_frame = frame_;
  const hir::GenerateId gen_id = NextGenerateId();

  std::vector<hir::CaseGenerateItem> items;
  std::optional<hir::StructuralScopeId> default_id;

  for (const auto* block : siblings) {
    switch (block->branchKind) {
      case slang::ast::GenerateBranchKind::CaseItem: {
        std::vector<hir::ExprId> labels;
        labels.reserve(block->caseItemExpressions.size());
        for (const auto* label_expr : block->caseItemExpressions) {
          auto label_expr_lowered = LowerExpr(*label_expr, frame);
          if (!label_expr_lowered) {
            return std::unexpected(std::move(label_expr_lowered.error()));
          }
          labels.push_back(AddExpr(*std::move(label_expr_lowered)));
        }
        auto item_id =
            AddChildScope(*module_, gen, gen_frame, gen_id, *block, frame);
        if (!item_id) return std::unexpected(std::move(item_id.error()));
        items.push_back(
            hir::CaseGenerateItem{
                .labels = std::move(labels), .scope = *item_id});
        break;
      }
      case slang::ast::GenerateBranchKind::CaseDefault: {
        if (default_id.has_value()) {
          throw InternalError(
              "ScopeLowerer::BuildCaseGenerate: case-generate has more than "
              "one default branch");
        }
        auto built =
            AddChildScope(*module_, gen, gen_frame, gen_id, *block, frame);
        if (!built) return std::unexpected(std::move(built.error()));
        default_id = *built;
        break;
      }
      default:
        throw InternalError(
            "ScopeLowerer::BuildCaseGenerate: unexpected branch kind in "
            "case-generate");
    }
  }

  gen.data = hir::CaseGenerate{
      .condition = cond_id,
      .items = std::move(items),
      .default_scope = default_id};
  return gen;
}

namespace {

// Lower the iter expression of a loop_generate header into the loop's
// next-value expression for its loop variable (LRM 27.5).
auto LowerLoopIterNextValue(
    ScopeLowerer& parent, ModuleLowerer& module,
    const slang::ast::Expression& iter, WalkFrame frame)
    -> diag::Result<hir::Expr> {
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(iter.sourceRange);

  if (iter.kind != slang::ast::ExpressionKind::Assignment) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "this generate iteration form is not supported yet",
        diag::UnsupportedCategory::kFeature);
  }

  const auto& assign = iter.as<slang::ast::AssignmentExpression>();
  if (!assign.op.has_value()) {
    return parent.LowerExpr(assign.right(), frame);
  }

  auto read = parent.LowerExpr(assign.left(), frame);
  if (!read) return std::unexpected(std::move(read.error()));
  const hir::ExprId read_id = parent.AddExpr(*std::move(read));

  const auto& bare_rhs = BareCompoundUserRhs(assign.right());
  auto rhs = parent.LowerExpr(bare_rhs, frame);
  if (!rhs) return std::unexpected(std::move(rhs.error()));
  const hir::ExprId rhs_id = parent.AddExpr(*std::move(rhs));

  auto type_id = module.GetTypeId(*iter.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::BinaryExpr{
              .op = LowerBinaryOp(*assign.op), .lhs = read_id, .rhs = rhs_id},
      .span = span,
  };
}

}  // namespace

auto ScopeLowerer::BuildLoopGenerate(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<hir::Generate> {
  const auto* loop_var_sym = array.loopVariable;
  if (loop_var_sym == nullptr || array.initialExpression == nullptr ||
      array.stopExpression == nullptr || array.iterExpression == nullptr) {
    throw InternalError(
        "ScopeLowerer::BuildLoopGenerate: GenerateBlockArraySymbol is "
        "missing loopVariable or bound header expressions");
  }

  const auto var_span =
      module_->SourceMapper().PointSpanOf(loop_var_sym->location);
  auto loop_var_type_or = module_->GetTypeId(loop_var_sym->getType(), var_span);
  if (!loop_var_type_or) {
    return std::unexpected(std::move(loop_var_type_or.error()));
  }

  const hir::LoopVarDeclId loop_var_id =
      AddLoopVarDecl(*loop_var_sym, *loop_var_type_or);

  auto initial_expr = LowerExpr(*array.initialExpression, frame);
  if (!initial_expr) return std::unexpected(std::move(initial_expr.error()));
  const hir::ExprId initial_id = AddExpr(*std::move(initial_expr));

  auto stop_expr = LowerExpr(*array.stopExpression, frame);
  if (!stop_expr) return std::unexpected(std::move(stop_expr.error()));
  const hir::ExprId stop_id = AddExpr(*std::move(stop_expr));

  auto iter_expr =
      LowerLoopIterNextValue(*this, *module_, *array.iterExpression, frame);
  if (!iter_expr) return std::unexpected(std::move(iter_expr.error()));
  const hir::ExprId iter_id = AddExpr(*std::move(iter_expr));

  hir::Generate gen{};
  hir::StructuralScope loop_scope_seed;
  loop_scope_seed.source_name = std::string{array.name};
  const hir::StructuralScopeId loop_scope_id =
      AddChildStructuralScope(gen, std::move(loop_scope_seed));
  module_->MapOwnedChildBinding(
      array, frame_,
      hir::DownwardHead{
          .child = hir::GenerateChildRef{
              .generate = NextGenerateId(), .scope = loop_scope_id}});

  if (!array.entries.empty()) {
    const auto& canonical_entry = *array.entries.front();
    const auto* body_param =
        DeriveLoopVariableSubstitution(array, canonical_entry);

    std::vector<ScopeEntryLoopVarBinding> body_bindings{
        ScopeEntryLoopVarBinding{
            .symbol = body_param,
            .home_frame = frame_,
            .loop_var = loop_var_id,
            .type = *loop_var_type_or,
        }};

    auto& loop_scope = gen.child_scopes.at(loop_scope_id.value);
    ScopeLowerer child(
        *module_, loop_scope, canonical_entry, std::move(body_bindings));
    auto r = child.Run(frame);
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
