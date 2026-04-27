#include "lower.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/numeric/SVInt.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "../type.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inspect.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerIntegerLiteralValue(
    const UnitLoweringFacts& unit_facts,
    const slang::ast::IntegerLiteral& literal) -> diag::Result<std::int64_t> {
  const auto value = literal.getValue().as<std::int64_t>();
  if (!value.has_value()) {
    return diag::Unsupported(
        unit_facts.SourceMapper().SpanOf(literal.sourceRange),
        diag::DiagCode::kUnsupportedIntegerLiteralWidth,
        "integer literal does not fit in a 64-bit signed integer",
        diag::UnsupportedCategory::kFeature);
  }
  return *value;
}

auto MakeLiteralExpr(std::int64_t v) -> hir::Expr {
  return hir::Expr{
      .data = hir::PrimaryExpr{.data = hir::IntegerLiteral{.value = v}}};
}

auto MakeRefExpr(hir::ValueRef ref) -> hir::Expr {
  return hir::Expr{
      .data = hir::PrimaryExpr{.data = hir::RefExpr{.target = std::move(ref)}}};
}

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> support::SystemSubroutineKind {
  switch (k) {
    case slang::ast::SubroutineKind::Function:
      return support::SystemSubroutineKind::kFunction;
    case slang::ast::SubroutineKind::Task:
      return support::SystemSubroutineKind::kTask;
  }
  throw support::InternalError(
      "FromSlangSubroutineKind: unknown SubroutineKind");
}

auto MakeReturnConventionType(
    UnitLoweringState& unit_state, support::ReturnConvention conv)
    -> hir::TypeId {
  switch (conv) {
    case support::ReturnConvention::kVoid:
      return unit_state.AddType(hir::TypeData{hir::VoidType{}});
  }
  throw support::InternalError(
      "MakeReturnConventionType: unknown ReturnConvention");
}

auto LowerNamedValueProc(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  if (named.symbol.kind != slang::ast::SymbolKind::Variable) {
    return diag::Unsupported(
        mapper.SpanOf(named.sourceRange),
        diag::DiagCode::kUnsupportedNonVariableNamedReference,
        "reference to non-variable declaration is not supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& var = named.symbol.as<slang::ast::VariableSymbol>();

  if (auto local = proc_state.LookupLocalVar(var)) {
    return MakeRefExpr(hir::LocalVarRef{.target = *local});
  }

  const auto binding = unit_state.LookupMemberVarBinding(var);
  if (!binding.has_value()) {
    throw support::InternalError(
        "LowerProcExpr: variable was not bound during scope lowering");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw support::InternalError(
        "LowerProcExpr: variable home frame is not on the current scope stack");
  }
  return MakeRefExpr(
      hir::MemberVarRef{
          .parent_scope_hops = *hops, .target = binding->local_id});
}

}  // namespace

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);

  auto lower_child = [&](const slang::ast::Expression& e) {
    return LowerProcExpr(unit_facts, unit_state, proc_state, stack, e);
  };
  auto append_child =
      [&](const slang::ast::Expression& e) -> diag::Result<hir::ExprId> {
    auto child = lower_child(e);
    if (!child) return std::unexpected(std::move(child.error()));
    return proc_state.AppendExpr(*std::move(child));
  };
  auto type_id_of =
      [&](const slang::ast::Expression& e) -> diag::Result<hir::TypeId> {
    auto td = LowerTypeData(*e.type, mapper.SpanOf(e.sourceRange));
    if (!td) return std::unexpected(std::move(td.error()));
    return unit_state.AddType(*std::move(td));
  };

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto v = LowerIntegerLiteralValue(
          unit_facts, expr.as<slang::ast::IntegerLiteral>());
      if (!v) return std::unexpected(std::move(v.error()));
      return MakeLiteralExpr(*v);
    }

    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValueProc(
          unit_facts, unit_state, proc_state, stack,
          expr.as<slang::ast::NamedValueExpression>());

    case slang::ast::ExpressionKind::BinaryOp: {
      const auto& bin = expr.as<slang::ast::BinaryExpression>();
      if (bin.op != slang::ast::BinaryOperator::Add) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedBinaryOperator,
            "only `+` is supported in this cut",
            diag::UnsupportedCategory::kOperation);
      }
      auto lhs_id = append_child(bin.left());
      if (!lhs_id) return std::unexpected(std::move(lhs_id.error()));
      auto rhs_id = append_child(bin.right());
      if (!rhs_id) return std::unexpected(std::move(rhs_id.error()));
      auto type_id = type_id_of(expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .data = hir::BinaryExpr{
              .op = hir::BinaryOp::kAdd,
              .lhs = *lhs_id,
              .rhs = *rhs_id,
              .type = *type_id}};
    }

    case slang::ast::ExpressionKind::Call: {
      const auto& call = expr.as<slang::ast::CallExpression>();

      std::vector<hir::ExprId> arg_ids;
      arg_ids.reserve(call.arguments().size());
      for (const auto* arg : call.arguments()) {
        auto id = append_child(*arg);
        if (!id) return std::unexpected(std::move(id.error()));
        arg_ids.push_back(*id);
      }

      if (call.isSystemCall()) {
        const auto& info = std::get<slang::ast::CallExpression::SystemCallInfo>(
            call.subroutine);
        const std::string_view name = info.subroutine->name;

        const auto* desc = support::FindSystemSubroutine(name);
        if (desc == nullptr) {
          throw support::InternalError(
              std::string{"AST->HIR call: unresolved system subroutine '"} +
              std::string{name} + "' after slang resolution");
        }
        const auto frontend_kind =
            FromSlangSubroutineKind(info.subroutine->kind);
        if (desc->kind != frontend_kind) {
          throw support::InternalError(
              std::string{
                  "AST->HIR call: registry/frontend kind mismatch for '"} +
              std::string{name} + "'");
        }
        if (!desc->arg_policy.Accepts(arg_ids.size())) {
          throw support::InternalError(
              std::string{
                  "AST->HIR call: arg count outside descriptor policy for '"} +
              std::string{name} + "'");
        }

        const auto result_type =
            MakeReturnConventionType(unit_state, desc->result_conv);
        return hir::Expr{
            .data = hir::CallExpr{
                .callee = hir::SystemSubroutineRef{.id = desc->id},
                .arguments = std::move(arg_ids),
                .result_type = result_type}};
      }

      const auto* sym =
          std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
      if (sym == nullptr) {
        throw support::InternalError(
            "AST->HIR call: user call missing resolved SubroutineSymbol");
      }
      const auto binding = unit_state.LookupSubroutineBinding(*sym);
      if (!binding.has_value()) {
        throw support::InternalError(
            "AST->HIR call: resolved user subroutine has no registered HIR "
            "binding");
      }
      const auto hops = stack.HopsTo(binding->owner_frame);
      if (!hops.has_value()) {
        throw support::InternalError(
            "AST->HIR call: user subroutine owner frame is not on the current "
            "scope stack");
      }
      auto result_type = type_id_of(expr);
      if (!result_type) {
        return std::unexpected(std::move(result_type.error()));
      }
      return hir::Expr{
          .data = hir::CallExpr{
              .callee =
                  hir::UserSubroutineRef{
                      .parent_scope_hops = *hops, .id = binding->local_id},
              .arguments = std::move(arg_ids),
              .result_type = *result_type}};
    }

    case slang::ast::ExpressionKind::Assignment: {
      const auto& as = expr.as<slang::ast::AssignmentExpression>();
      if (as.isNonBlocking()) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedNonBlockingAssignment,
            "non-blocking assignments are not supported yet",
            diag::UnsupportedCategory::kFeature);
      }
      if (as.op.has_value()) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedCompoundAssignment,
            "compound assignments are not supported yet",
            diag::UnsupportedCategory::kFeature);
      }

      auto lhs_expr = lower_child(as.left());
      if (!lhs_expr) return std::unexpected(std::move(lhs_expr.error()));
      if (!hir::AsAssignableRef(*lhs_expr).has_value()) {
        return diag::Unsupported(
            mapper.SpanOf(as.left().sourceRange),
            diag::DiagCode::kUnsupportedAssignmentTarget,
            "assignment target is not supported yet",
            diag::UnsupportedCategory::kFeature);
      }
      const hir::ExprId lhs_id = proc_state.AppendExpr(*std::move(lhs_expr));

      auto rhs_id = append_child(as.right());
      if (!rhs_id) return std::unexpected(std::move(rhs_id.error()));
      auto type_id = type_id_of(expr);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .data =
              hir::AssignExpr{.lhs = lhs_id, .rhs = *rhs_id, .type = *type_id}};
    }

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "this expression form is not supported yet",
          diag::UnsupportedCategory::kOperation);
  }
}

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto v = LowerIntegerLiteralValue(
          unit_facts, expr.as<slang::ast::IntegerLiteral>());
      if (!v) return std::unexpected(std::move(v.error()));
      return MakeLiteralExpr(*v);
    }
    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
          "this structural expression form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

}  // namespace lyra::lowering::ast_to_hir
