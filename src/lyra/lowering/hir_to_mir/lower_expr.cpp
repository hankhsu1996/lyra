#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inspect.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
    case hir::BinaryOp::kLessThan:
      return mir::BinaryOp::kLessThan;
  }
  throw InternalError("LowerBinaryOp: unknown HIR BinaryOp");
}

auto LowerTimeScale(hir::TimeScale s) -> mir::TimeScale {
  switch (s) {
    case hir::TimeScale::kFs:
      return mir::TimeScale::kFs;
    case hir::TimeScale::kPs:
      return mir::TimeScale::kPs;
    case hir::TimeScale::kNs:
      return mir::TimeScale::kNs;
    case hir::TimeScale::kUs:
      return mir::TimeScale::kUs;
    case hir::TimeScale::kMs:
      return mir::TimeScale::kMs;
    case hir::TimeScale::kS:
      return mir::TimeScale::kS;
  }
  throw InternalError("LowerTimeScale: unknown HIR TimeScale");
}

namespace {

auto LowerSignedness(hir::Signedness s) -> mir::Signedness {
  switch (s) {
    case hir::Signedness::kSigned:
      return mir::Signedness::kSigned;
    case hir::Signedness::kUnsigned:
      return mir::Signedness::kUnsigned;
  }
  throw InternalError("LowerSignedness: unknown HIR Signedness");
}

auto LowerStateKind(hir::IntegralStateKind k) -> mir::IntegralStateKind {
  switch (k) {
    case hir::IntegralStateKind::kTwoState:
      return mir::IntegralStateKind::kTwoState;
    case hir::IntegralStateKind::kFourState:
      return mir::IntegralStateKind::kFourState;
  }
  throw InternalError("LowerStateKind: unknown HIR IntegralStateKind");
}

auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant {
  return mir::IntegralConstant{
      .value_words = c.value_words,
      .state_words = c.state_words,
      .width = c.width,
      .signedness = LowerSignedness(c.signedness),
      .state_kind = LowerStateKind(c.state_kind),
  };
}

auto LowerHirConversionKind(hir::ConversionKind k) -> mir::ConversionKind {
  switch (k) {
    case hir::ConversionKind::kImplicit:
      return mir::ConversionKind::kImplicit;
    case hir::ConversionKind::kPropagated:
      return mir::ConversionKind::kPropagated;
    case hir::ConversionKind::kStreamingConcat:
      return mir::ConversionKind::kStreamingConcat;
    case hir::ConversionKind::kExplicit:
      return mir::ConversionKind::kExplicit;
    case hir::ConversionKind::kBitstreamCast:
      return mir::ConversionKind::kBitstreamCast;
  }
  throw InternalError("LowerHirConversionKind: unknown HIR ConversionKind");
}

auto LowerMemberVarRefExpr(
    const ClassLoweringState& class_state, const hir::MemberVarRef& m,
    mir::TypeId type) -> mir::Expr {
  const mir::MemberVarId mir_id =
      class_state.TranslateMemberVar(m.parent_scope_hops, m.target);
  return mir::Expr{.data = mir::MemberVarRef{.target = mir_id}, .type = type};
}

auto LowerLocalVarRefExpr(
    const ProcessLoweringState& proc_state, const hir::LocalVarRef& l,
    mir::TypeId type) -> mir::Expr {
  const mir::LocalVarRef ref = proc_state.TranslateLocalVar(l.target);
  return mir::Expr{.data = ref, .type = type};
}

auto LowerLoopVarRefExpr(
    const ConstructorLoweringState& ctor_state, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  const mir::LocalVarRef ref = ctor_state.TranslateLoopVar(lv.target);
  return mir::Expr{.data = ref, .type = type};
}

auto LowerRefAsLvalue(
    const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const hir::ValueRef& ref)
    -> mir::Lvalue {
  return std::visit(
      Overloaded{
          [&](const hir::MemberVarRef& m) -> mir::Lvalue {
            return mir::MemberVarRef{
                .target = class_state.TranslateMemberVar(
                    m.parent_scope_hops, m.target)};
          },
          [&](const hir::LocalVarRef& l) -> mir::Lvalue {
            return proc_state.TranslateLocalVar(l.target);
          },
          [](const hir::LoopVarRef&) -> mir::Lvalue {
            throw InternalError(
                "LowerRefAsLvalue (process): HIR LoopVarRef does not appear "
                "in process bodies");
          },
      },
      ref);
}

auto LowerRefAsLvalue(
    const ClassLoweringState& class_state,
    const ConstructorLoweringState& ctor_state, const hir::ValueRef& ref)
    -> mir::Lvalue {
  return std::visit(
      Overloaded{
          [&](const hir::MemberVarRef& m) -> mir::Lvalue {
            return mir::MemberVarRef{
                .target = class_state.TranslateMemberVar(
                    m.parent_scope_hops, m.target)};
          },
          [](const hir::LocalVarRef&) -> mir::Lvalue {
            throw InternalError(
                "LowerRefAsLvalue (constructor): HIR LocalVarRef does not "
                "appear in constructor bodies");
          },
          [&](const hir::LoopVarRef& lv) -> mir::Lvalue {
            return ctor_state.TranslateLoopVar(lv.target);
          },
      },
      ref);
}

auto LowerUserCallee(
    const ClassLoweringState& class_state, const hir::UserSubroutineRef& u)
    -> mir::Callee {
  return class_state.TranslateUserSubroutine(u.parent_scope_hops, u.id);
}

auto LowerPrimaryExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, const hir::PrimaryExpr& p,
    mir::TypeId type) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::IntegerLiteral{
                        .value = LowerHirIntegralConstant(i.value)},
                .type = type};
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return mir::Expr{
                .data = mir::StringLiteral{.value = s.value}, .type = type};
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::TimeLiteral{
                        .value = t.value, .scale = LowerTimeScale(t.scale)},
                .type = unit_state.Builtins().realtime};
          },
          [&](const hir::RefExpr& r) -> mir::Expr {
            return std::visit(
                Overloaded{
                    [&](const hir::MemberVarRef& m) -> mir::Expr {
                      return LowerMemberVarRefExpr(class_state, m, type);
                    },
                    [&](const hir::LocalVarRef& l) -> mir::Expr {
                      return LowerLocalVarRefExpr(proc_state, l, type);
                    },
                    [](const hir::LoopVarRef&) -> mir::Expr {
                      throw InternalError(
                          "LowerPrimaryExpr (process): HIR LoopVarRef does "
                          "not appear in process bodies");
                    },
                },
                r.target);
          },
      },
      p.data);
  (void)unit_state;
}

auto LowerPrimaryExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ConstructorLoweringState& ctor_state, const hir::PrimaryExpr& p,
    mir::TypeId type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> diag::Result<mir::Expr> {
            return mir::Expr{
                .data =
                    mir::IntegerLiteral{
                        .value = LowerHirIntegralConstant(i.value)},
                .type = type};
          },
          [&](const hir::StringLiteral& s) -> diag::Result<mir::Expr> {
            return mir::Expr{
                .data = mir::StringLiteral{.value = s.value}, .type = type};
          },
          [&](const hir::TimeLiteral& t) -> diag::Result<mir::Expr> {
            return mir::Expr{
                .data =
                    mir::TimeLiteral{
                        .value = t.value, .scale = LowerTimeScale(t.scale)},
                .type = unit_state.Builtins().realtime};
          },
          [&](const hir::RefExpr& r) -> diag::Result<mir::Expr> {
            return std::visit(
                Overloaded{
                    [&](const hir::MemberVarRef& m) -> diag::Result<mir::Expr> {
                      return LowerMemberVarRefExpr(class_state, m, type);
                    },
                    [](const hir::LocalVarRef&) -> diag::Result<mir::Expr> {
                      return diag::Unsupported(
                          diag::DiagCode::kUnsupportedStructuralExpressionForm,
                          "local variable references are not allowed in "
                          "constructor expressions",
                          diag::UnsupportedCategory::kFeature);
                    },
                    [&](const hir::LoopVarRef& lv) -> diag::Result<mir::Expr> {
                      return LowerLoopVarRefExpr(ctor_state, lv, type);
                    },
                },
                r.target);
          },
      },
      p.data);
  (void)unit_state;
}

}  // namespace

auto LowerExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, BodyLoweringState& body_state,
    const hir::Process& hir_process, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = unit_state.TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerPrimaryExpr(
                unit_state, class_state, proc_state, p, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            auto lhs_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_process,
                hir_process.exprs.at(b.lhs.value));
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            const mir::ExprId lhs_id = body_state.AddExpr(*std::move(lhs_or));
            auto rhs_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_process,
                hir_process.exprs.at(b.rhs.value));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id = body_state.AddExpr(*std::move(rhs_or));
            return mir::Expr{
                .data =
                    mir::BinaryExpr{
                        .op = LowerBinaryOp(b.op),
                        .lhs = lhs_id,
                        .rhs = rhs_id},
                .type = result_type};
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            const auto& lhs_expr = hir_process.exprs.at(a.lhs.value);
            const auto ref = hir::AsAssignableRef(lhs_expr);
            if (!ref.has_value()) {
              throw InternalError(
                  "AssignExpr lhs is not assignable (AST->HIR invariant)");
            }
            auto rhs_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_process,
                hir_process.exprs.at(a.rhs.value));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id = body_state.AddExpr(*std::move(rhs_or));
            return mir::Expr{
                .data =
                    mir::AssignExpr{
                        .target = LowerRefAsLvalue(
                            class_state, proc_state, ref->get().target),
                        .value = rhs_id},
                .type = result_type};
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            auto operand_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_process,
                hir_process.exprs.at(cv.operand.value));
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const mir::ExprId operand_id =
                body_state.AddExpr(*std::move(operand_or));
            return mir::Expr{
                .data =
                    mir::ConversionExpr{
                        .operand = operand_id,
                        .kind = LowerHirConversionKind(cv.kind)},
                .type = result_type};
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return std::visit(
                Overloaded{
                    [&](const hir::SystemSubroutineRef& sys)
                        -> diag::Result<mir::Expr> {
                      return LowerSystemSubroutineCall(
                          unit_state, class_state, proc_state, body_state,
                          hir_process, c, sys, expr.span);
                    },
                    [&](const hir::UserSubroutineRef& usr)
                        -> diag::Result<mir::Expr> {
                      std::vector<mir::ExprId> args;
                      args.reserve(c.arguments.size());
                      for (const auto arg : c.arguments) {
                        auto arg_or = LowerExpr(
                            unit_state, class_state, proc_state, body_state,
                            hir_process, hir_process.exprs.at(arg.value));
                        if (!arg_or)
                          return std::unexpected(std::move(arg_or.error()));
                        args.push_back(body_state.AddExpr(*std::move(arg_or)));
                      }
                      return mir::Expr{
                          .data =
                              mir::CallExpr{
                                  .callee = LowerUserCallee(class_state, usr),
                                  .arguments = std::move(args)},
                          .type = result_type};
                    },
                },
                c.callee);
          },
      },
      expr.data);
}

auto LowerExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ConstructorLoweringState& ctor_state, BodyLoweringState& body_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = unit_state.TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerPrimaryExpr(
                unit_state, class_state, ctor_state, p, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            auto lhs_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state, scope,
                scope.GetExpr(b.lhs));
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            const mir::ExprId lhs_id = body_state.AddExpr(*std::move(lhs_or));
            auto rhs_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state, scope,
                scope.GetExpr(b.rhs));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id = body_state.AddExpr(*std::move(rhs_or));
            return mir::Expr{
                .data =
                    mir::BinaryExpr{
                        .op = LowerBinaryOp(b.op),
                        .lhs = lhs_id,
                        .rhs = rhs_id},
                .type = result_type};
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            const auto& lhs_expr = scope.GetExpr(a.lhs);
            const auto ref = hir::AsAssignableRef(lhs_expr);
            if (!ref.has_value()) {
              throw InternalError(
                  "AssignExpr lhs is not assignable (AST->HIR invariant)");
            }
            auto rhs_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state, scope,
                scope.GetExpr(a.rhs));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id = body_state.AddExpr(*std::move(rhs_or));
            return mir::Expr{
                .data =
                    mir::AssignExpr{
                        .target = LowerRefAsLvalue(
                            class_state, ctor_state, ref->get().target),
                        .value = rhs_id},
                .type = result_type};
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            auto operand_or = LowerExpr(
                unit_state, class_state, ctor_state, body_state, scope,
                scope.GetExpr(cv.operand));
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const mir::ExprId operand_id =
                body_state.AddExpr(*std::move(operand_or));
            return mir::Expr{
                .data =
                    mir::ConversionExpr{
                        .operand = operand_id,
                        .kind = LowerHirConversionKind(cv.kind)},
                .type = result_type};
          },
          [](const hir::CallExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "calls are not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
