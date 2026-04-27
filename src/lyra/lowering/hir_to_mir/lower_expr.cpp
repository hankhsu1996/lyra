#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <variant>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inspect.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
  }
  throw support::InternalError("LowerBinaryOp: unknown HIR BinaryOp");
}

namespace {

auto ResolveMemberVarRef(
    const ClassLoweringState& class_state, const ScopeStack& /*stack*/,
    const hir::MemberVarRef& m) -> mir::MemberVarId {
  return class_state.LookupMemberVar(m.parent_scope_hops, m.target);
}

auto LowerRefAsLvalue(
    const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const ScopeStack& stack,
    const hir::ValueRef& ref) -> mir::Lvalue {
  return std::visit(
      support::Overloaded{
          [&](const hir::MemberVarRef& m) -> mir::Lvalue {
            return mir::MemberVarRef{
                .target = ResolveMemberVarRef(class_state, stack, m)};
          },
          [&](const hir::LocalVarRef& l) -> mir::Lvalue {
            return mir::LocalVarRef{
                .target = proc_state.TranslateLocalVar(l.target)};
          },
      },
      ref);
}

}  // namespace

auto LowerProcessExprData(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const BodyLoweringState& body_state,
    const ScopeStack& stack, const hir::Process& hir_process,
    const hir::ExprData& data) -> mir::ExprData {
  return std::visit(
      support::Overloaded{
          [&](const hir::PrimaryExpr& p) -> mir::ExprData {
            return std::visit(
                support::Overloaded{
                    [](const hir::IntegerLiteral& i) -> mir::ExprData {
                      return mir::IntegerLiteral{.value = i.value};
                    },
                    [&](const hir::RefExpr& r) -> mir::ExprData {
                      return std::visit(
                          support::Overloaded{
                              [&](const hir::MemberVarRef& m) -> mir::ExprData {
                                return mir::MemberVarRef{
                                    .target = ResolveMemberVarRef(
                                        class_state, stack, m)};
                              },
                              [&](const hir::LocalVarRef& l) -> mir::ExprData {
                                return mir::LocalVarRef{
                                    .target =
                                        proc_state.TranslateLocalVar(l.target)};
                              },
                          },
                          r.target);
                    },
                },
                p.data);
          },
          [&](const hir::BinaryExpr& b) -> mir::ExprData {
            return mir::BinaryExpr{
                .op = LowerBinaryOp(b.op),
                .lhs = body_state.TranslateExpr(b.lhs),
                .rhs = body_state.TranslateExpr(b.rhs),
                .type = unit_state.TranslateType(b.type)};
          },
          [&](const hir::AssignExpr& a) -> mir::ExprData {
            const auto& lhs_expr = hir_process.exprs.at(a.lhs.value);
            const auto ref = hir::AsAssignableRef(lhs_expr);
            if (!ref.has_value()) {
              throw support::InternalError(
                  "AssignExpr lhs is not assignable (AST->HIR invariant)");
            }
            return mir::AssignExpr{
                .target = LowerRefAsLvalue(
                    class_state, proc_state, stack, ref->get().target),
                .value = body_state.TranslateExpr(a.rhs),
                .type = unit_state.TranslateType(a.type)};
          },
      },
      data);
}

auto LowerStructuralExprData(const hir::ExprData& data)
    -> diag::Result<mir::ExprData> {
  return std::visit(
      support::Overloaded{
          [](const hir::PrimaryExpr& p) -> diag::Result<mir::ExprData> {
            return std::visit(
                support::Overloaded{
                    [](const hir::IntegerLiteral& i)
                        -> diag::Result<mir::ExprData> {
                      return mir::ExprData{
                          mir::IntegerLiteral{.value = i.value}};
                    },
                    [](const hir::RefExpr&) -> diag::Result<mir::ExprData> {
                      return diag::Unsupported(
                          diag::DiagCode::kUnsupportedStructuralExpressionForm,
                          "this structural expression form is not "
                          "supported yet",
                          diag::UnsupportedCategory::kFeature);
                    },
                },
                p.data);
          },
          [](const hir::BinaryExpr&) -> diag::Result<mir::ExprData> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "this structural expression form is not supported yet",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::AssignExpr&) -> diag::Result<mir::ExprData> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "this structural expression form is not supported yet",
                diag::UnsupportedCategory::kFeature);
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
