#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <utility>
#include <variant>
#include <vector>

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
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"
#include "lyra/support/system_subroutine.hpp"

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
    const ClassLoweringState& class_state, const hir::MemberVarRef& m)
    -> mir::MemberVarId {
  return class_state.LookupMemberVar(m.parent_scope_hops, m.target);
}

auto LowerRefAsLvalue(
    const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const hir::ValueRef& ref)
    -> mir::Lvalue {
  return std::visit(
      support::Overloaded{
          [&](const hir::MemberVarRef& m) -> mir::Lvalue {
            return mir::MemberVarRef{
                .target = ResolveMemberVarRef(class_state, m)};
          },
          [&](const hir::LocalVarRef& l) -> mir::Lvalue {
            return mir::LocalVarRef{
                .target = proc_state.TranslateLocalVar(l.target)};
          },
      },
      ref);
}

auto NormalizeSemanticToBuiltinOp(
    const support::SystemSubroutineSemantic& semantic) -> mir::BuiltinOp {
  return std::visit(
      support::Overloaded{
          [](const support::PrintSystemSubroutineInfo& info) -> mir::BuiltinOp {
            return mir::PrintBuiltinInfo{
                .radix = info.radix,
                .append_newline = info.append_newline,
                .is_strobe = info.is_strobe,
                .sink_kind = info.sink_kind};
          },
      },
      semantic);
}

auto LowerCallee(
    const ClassLoweringState& class_state, const hir::SubroutineRef& callee)
    -> mir::Callee {
  return std::visit(
      support::Overloaded{
          [&](const hir::UserSubroutineRef& u) -> mir::Callee {
            return class_state.LookupUserSubroutine(u.parent_scope_hops, u.id);
          },
          [](const hir::SystemSubroutineRef& s) -> mir::Callee {
            const auto& desc = support::LookupSystemSubroutine(s.id);
            return NormalizeSemanticToBuiltinOp(desc.semantic);
          },
      },
      callee);
}

}  // namespace

auto LowerProcessExprData(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const BodyLoweringState& body_state,
    const hir::Process& hir_process, const hir::ExprData& data)
    -> mir::ExprData {
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
                                    .target =
                                        ResolveMemberVarRef(class_state, m)};
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
                    class_state, proc_state, ref->get().target),
                .value = body_state.TranslateExpr(a.rhs),
                .type = unit_state.TranslateType(a.type)};
          },
          [&](const hir::CallExpr& c) -> mir::ExprData {
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size());
            for (const auto arg : c.arguments) {
              args.push_back(body_state.TranslateExpr(arg));
            }
            return mir::CallExpr{
                .callee = LowerCallee(class_state, c.callee),
                .arguments = std::move(args),
                .result_type = unit_state.TranslateType(c.result_type)};
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
          [](const hir::CallExpr&) -> diag::Result<mir::ExprData> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "calls are not allowed in structural expressions",
                diag::UnsupportedCategory::kFeature);
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
