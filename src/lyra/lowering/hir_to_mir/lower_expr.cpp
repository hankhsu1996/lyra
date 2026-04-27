#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inspect.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
    case hir::BinaryOp::kLessThan:
      throw InternalError(
          "LowerBinaryOp: kLessThan only appears in loop-generate header "
          "expressions, which are rejected before HIR-to-MIR");
  }
  throw InternalError("LowerBinaryOp: unknown HIR BinaryOp");
}

namespace {

auto LowerMemberVarRefExpr(
    const ClassLoweringState& class_state, const hir::MemberVarRef& m)
    -> mir::Expr {
  const mir::MemberVarId mir_id =
      class_state.LookupMemberVar(m.parent_scope_hops, m.target);
  const auto& member = class_state.GetMemberVar(m.parent_scope_hops, mir_id);
  const auto* value = std::get_if<mir::ValueMember>(&member.kind);
  if (value == nullptr) {
    throw InternalError("LowerMemberVarRef: member is not a value member");
  }
  return mir::Expr{
      .data = mir::MemberVarRef{.target = mir_id}, .type = value->type};
}

auto LowerLocalVarRefExpr(
    const ProcessLoweringState& proc_state, const hir::LocalVarRef& l)
    -> mir::Expr {
  const mir::LocalVarId mir_id = proc_state.TranslateLocalVar(l.target);
  return mir::Expr{
      .data = mir::LocalVarRef{.target = mir_id},
      .type = proc_state.GetLocalVar(mir_id).type};
}

auto LowerRefAsLvalue(
    const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const hir::ValueRef& ref)
    -> mir::Lvalue {
  return std::visit(
      Overloaded{
          [&](const hir::MemberVarRef& m) -> mir::Lvalue {
            return mir::MemberVarRef{
                .target =
                    class_state.LookupMemberVar(m.parent_scope_hops, m.target)};
          },
          [&](const hir::LocalVarRef& l) -> mir::Lvalue {
            return mir::LocalVarRef{
                .target = proc_state.TranslateLocalVar(l.target)};
          },
          [](const hir::LoopVarRef&) -> mir::Lvalue {
            throw InternalError(
                "LowerRefAsLvalue: loop-variable references only appear in "
                "loop-generate header expressions, which are rejected before "
                "HIR-to-MIR");
          },
      },
      ref);
}

auto LowerCallee(
    const ClassLoweringState& class_state, const hir::SubroutineRef& callee)
    -> mir::Callee {
  return std::visit(
      Overloaded{
          [&](const hir::UserSubroutineRef& u) -> mir::Callee {
            return class_state.LookupUserSubroutine(u.parent_scope_hops, u.id);
          },
          [](const hir::SystemSubroutineRef&) -> mir::Callee {
            throw InternalError(
                "system subroutine call reached expression lowering");
          },
      },
      callee);
}

auto LowerPrimaryExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const hir::PrimaryExpr& p)
    -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return mir::Expr{
                .data = mir::IntegerLiteral{.value = i.value},
                .type = unit_state.Builtins().int32};
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return mir::Expr{
                .data = mir::StringLiteral{.value = s.value},
                .type = unit_state.Builtins().string};
          },
          [&](const hir::RefExpr& r) -> mir::Expr {
            return std::visit(
                Overloaded{
                    [&](const hir::MemberVarRef& m) -> mir::Expr {
                      return LowerMemberVarRefExpr(class_state, m);
                    },
                    [&](const hir::LocalVarRef& l) -> mir::Expr {
                      return LowerLocalVarRefExpr(proc_state, l);
                    },
                    [](const hir::LoopVarRef&) -> mir::Expr {
                      throw InternalError(
                          "LowerPrimaryExpr: loop-variable references only "
                          "appear in loop-generate header expressions, which "
                          "are rejected before HIR-to-MIR");
                    },
                },
                r.target);
          },
      },
      p.data);
}

}  // namespace

auto LowerProcessExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const BodyLoweringState& body_state,
    const hir::Process& hir_process, const hir::Expr& expr) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> mir::Expr {
            return LowerPrimaryExpr(unit_state, class_state, proc_state, p);
          },
          [&](const hir::BinaryExpr& b) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::BinaryExpr{
                        .op = LowerBinaryOp(b.op),
                        .lhs = body_state.TranslateExpr(b.lhs),
                        .rhs = body_state.TranslateExpr(b.rhs)},
                .type = unit_state.TranslateType(b.type)};
          },
          [&](const hir::AssignExpr& a) -> mir::Expr {
            const auto& lhs_expr = hir_process.exprs.at(a.lhs.value);
            const auto ref = hir::AsAssignableRef(lhs_expr);
            if (!ref.has_value()) {
              throw InternalError(
                  "AssignExpr lhs is not assignable (AST->HIR invariant)");
            }
            return mir::Expr{
                .data =
                    mir::AssignExpr{
                        .target = LowerRefAsLvalue(
                            class_state, proc_state, ref->get().target),
                        .value = body_state.TranslateExpr(a.rhs)},
                .type = unit_state.TranslateType(a.type)};
          },
          [&](const hir::CallExpr& c) -> mir::Expr {
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size());
            for (const auto arg : c.arguments) {
              args.push_back(body_state.TranslateExpr(arg));
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = LowerCallee(class_state, c.callee),
                        .arguments = std::move(args)},
                .type = unit_state.TranslateType(c.result_type)};
          },
      },
      expr.data);
}

auto LowerStructuralExpr(
    const UnitLoweringState& unit_state, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return std::visit(
                Overloaded{
                    [&](const hir::IntegerLiteral& i)
                        -> diag::Result<mir::Expr> {
                      return mir::Expr{
                          .data = mir::IntegerLiteral{.value = i.value},
                          .type = unit_state.Builtins().int32};
                    },
                    [&](const hir::StringLiteral& s)
                        -> diag::Result<mir::Expr> {
                      return mir::Expr{
                          .data = mir::StringLiteral{.value = s.value},
                          .type = unit_state.Builtins().string};
                    },
                    [](const hir::RefExpr&) -> diag::Result<mir::Expr> {
                      return diag::Unsupported(
                          diag::DiagCode::kUnsupportedStructuralExpressionForm,
                          "this structural expression form is not "
                          "supported yet",
                          diag::UnsupportedCategory::kFeature);
                    },
                },
                p.data);
          },
          [](const hir::BinaryExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "this structural expression form is not supported yet",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::AssignExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "this structural expression form is not supported yet",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::CallExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "calls are not allowed in structural expressions",
                diag::UnsupportedCategory::kFeature);
          },
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
