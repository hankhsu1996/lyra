#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <cstdint>
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
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Selects how a HIR `LoopVarRef` lowers when reached from a constructor
// expression. The choice is set by the caller via the public
// `LowerProceduralExpr` / `LowerStructuralExpr` wrappers; it is never
// inferred from runtime state inside the recursion.
enum class LoopVarLoweringMode : std::uint8_t {
  kProceduralInduction,
  kStructuralParam,
};

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
    case hir::BinaryOp::kSub:
      return mir::BinaryOp::kSub;
    case hir::BinaryOp::kMul:
      return mir::BinaryOp::kMul;
    case hir::BinaryOp::kDiv:
      return mir::BinaryOp::kDiv;
    case hir::BinaryOp::kMod:
      return mir::BinaryOp::kMod;
    case hir::BinaryOp::kPower:
      return mir::BinaryOp::kPower;
    case hir::BinaryOp::kBitwiseAnd:
      return mir::BinaryOp::kBitwiseAnd;
    case hir::BinaryOp::kBitwiseOr:
      return mir::BinaryOp::kBitwiseOr;
    case hir::BinaryOp::kBitwiseXor:
      return mir::BinaryOp::kBitwiseXor;
    case hir::BinaryOp::kBitwiseXnor:
      return mir::BinaryOp::kBitwiseXnor;
    case hir::BinaryOp::kEquality:
      return mir::BinaryOp::kEquality;
    case hir::BinaryOp::kInequality:
      return mir::BinaryOp::kInequality;
    case hir::BinaryOp::kCaseEquality:
      return mir::BinaryOp::kCaseEquality;
    case hir::BinaryOp::kCaseInequality:
      return mir::BinaryOp::kCaseInequality;
    case hir::BinaryOp::kWildcardEquality:
      return mir::BinaryOp::kWildcardEquality;
    case hir::BinaryOp::kWildcardInequality:
      return mir::BinaryOp::kWildcardInequality;
    case hir::BinaryOp::kGreaterEqual:
      return mir::BinaryOp::kGreaterEqual;
    case hir::BinaryOp::kGreaterThan:
      return mir::BinaryOp::kGreaterThan;
    case hir::BinaryOp::kLessEqual:
      return mir::BinaryOp::kLessEqual;
    case hir::BinaryOp::kLessThan:
      return mir::BinaryOp::kLessThan;
    case hir::BinaryOp::kLogicalAnd:
      return mir::BinaryOp::kLogicalAnd;
    case hir::BinaryOp::kLogicalOr:
      return mir::BinaryOp::kLogicalOr;
    case hir::BinaryOp::kLogicalImplication:
      return mir::BinaryOp::kLogicalImplication;
    case hir::BinaryOp::kLogicalEquivalence:
      return mir::BinaryOp::kLogicalEquivalence;
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return mir::BinaryOp::kShiftLeft;
    case hir::BinaryOp::kLogicalShiftRight:
      return mir::BinaryOp::kLogicalShiftRight;
    case hir::BinaryOp::kArithmeticShiftRight:
      return mir::BinaryOp::kArithmeticShiftRight;
  }
  throw InternalError("LowerBinaryOp: unknown HIR BinaryOp");
}

auto LowerUnaryOp(hir::UnaryOp op) -> mir::UnaryOp {
  switch (op) {
    case hir::UnaryOp::kPlus:
      return mir::UnaryOp::kPlus;
    case hir::UnaryOp::kMinus:
      return mir::UnaryOp::kMinus;
    case hir::UnaryOp::kBitwiseNot:
      return mir::UnaryOp::kBitwiseNot;
    case hir::UnaryOp::kLogicalNot:
      return mir::UnaryOp::kLogicalNot;
    case hir::UnaryOp::kReductionAnd:
      return mir::UnaryOp::kReductionAnd;
    case hir::UnaryOp::kReductionOr:
      return mir::UnaryOp::kReductionOr;
    case hir::UnaryOp::kReductionXor:
      return mir::UnaryOp::kReductionXor;
    case hir::UnaryOp::kReductionNand:
      return mir::UnaryOp::kReductionNand;
    case hir::UnaryOp::kReductionNor:
      return mir::UnaryOp::kReductionNor;
    case hir::UnaryOp::kReductionXnor:
      return mir::UnaryOp::kReductionXnor;
  }
  throw InternalError("LowerUnaryOp: unknown HIR UnaryOp");
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

auto LowerStructuralVarRefExpr(
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralVarRef& m, mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = scope_state.TranslateStructuralVar(m.hops, m.var), .type = type};
}

auto LowerProceduralVarRefExpr(
    const ProcessLoweringState& proc_state, const hir::ProceduralVarRef& l,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = proc_state.TranslateProceduralVar(l.var), .type = type};
}

auto LowerLoopVarRefExpr(
    const ConstructorLoweringState& ctor_state, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = ctor_state.TranslateLoopVar(lv.loop_var), .type = type};
}

auto LowerStructuralParamRefExpr(
    const StructuralScopeLoweringState& scope_state, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data =
          scope_state.TranslateLoopVarAsStructuralParam(lv.hops, lv.loop_var),
      .type = type};
}

auto LowerRefAsLvalue(
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state, const hir::ValueRef& ref)
    -> mir::Lvalue {
  return std::visit(
      Overloaded{
          [&](const hir::StructuralVarRef& m) -> mir::Lvalue {
            return scope_state.TranslateStructuralVar(m.hops, m.var);
          },
          [&](const hir::ProceduralVarRef& l) -> mir::Lvalue {
            return proc_state.TranslateProceduralVar(l.var);
          },
          [](const hir::LoopVarRef&) -> mir::Lvalue {
            throw InternalError(
                "LowerRefAsLvalue (process): HIR LoopVarRef must not appear "
                "as an lvalue; AST->HIR rejects assignment to a loop variable "
                "via AsAssignableRef()");
          },
      },
      ref);
}

auto LowerRefAsLvalue(
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state, const hir::ValueRef& ref,
    LoopVarLoweringMode loop_var_mode) -> mir::Lvalue {
  return std::visit(
      Overloaded{
          [&](const hir::StructuralVarRef& m) -> mir::Lvalue {
            return scope_state.TranslateStructuralVar(m.hops, m.var);
          },
          [](const hir::ProceduralVarRef&) -> mir::Lvalue {
            throw InternalError(
                "LowerRefAsLvalue (constructor): HIR ProceduralVarRef does "
                "not appear in constructor bodies");
          },
          [&](const hir::LoopVarRef& lv) -> mir::Lvalue {
            if (loop_var_mode != LoopVarLoweringMode::kProceduralInduction) {
              throw InternalError(
                  "LowerRefAsLvalue (constructor): HIR LoopVarRef as lvalue "
                  "is only valid in for-stmt header context "
                  "(kProceduralInduction); StructuralParamRef is immutable");
            }
            if (ctor_state == nullptr) {
              throw InternalError(
                  "LowerRefAsLvalue: kProceduralInduction mode requires a "
                  "non-null ConstructorLoweringState");
            }
            return ctor_state->TranslateLoopVar(lv.loop_var);
          },
      },
      ref);
}

auto LowerUserCallee(
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralSubroutineRef& u) -> mir::Callee {
  return scope_state.TranslateStructuralSubroutine(u.hops, u.subroutine);
}

auto LowerPrimaryExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
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
                    [&](const hir::StructuralVarRef& m) -> mir::Expr {
                      return LowerStructuralVarRefExpr(scope_state, m, type);
                    },
                    [&](const hir::ProceduralVarRef& l) -> mir::Expr {
                      return LowerProceduralVarRefExpr(proc_state, l, type);
                    },
                    [&](const hir::LoopVarRef& lv) -> mir::Expr {
                      return LowerStructuralParamRefExpr(scope_state, lv, type);
                    },
                },
                r.target);
          },
      },
      p.data);
}

auto LowerPrimaryExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state, const hir::PrimaryExpr& p,
    mir::TypeId type, LoopVarLoweringMode loop_var_mode)
    -> diag::Result<mir::Expr> {
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
                    [&](const hir::StructuralVarRef& m)
                        -> diag::Result<mir::Expr> {
                      return LowerStructuralVarRefExpr(scope_state, m, type);
                    },
                    [](const hir::ProceduralVarRef&)
                        -> diag::Result<mir::Expr> {
                      return diag::Unsupported(
                          diag::DiagCode::kUnsupportedStructuralExpressionForm,
                          "procedural variable references are not allowed in "
                          "constructor expressions",
                          diag::UnsupportedCategory::kFeature);
                    },
                    [&](const hir::LoopVarRef& lv) -> diag::Result<mir::Expr> {
                      if (loop_var_mode ==
                          LoopVarLoweringMode::kProceduralInduction) {
                        if (ctor_state == nullptr) {
                          throw InternalError(
                              "LowerPrimaryExpr: kProceduralInduction mode "
                              "requires a non-null ConstructorLoweringState");
                        }
                        return LowerLoopVarRefExpr(*ctor_state, lv, type);
                      }
                      return LowerStructuralParamRefExpr(scope_state, lv, type);
                    },
                },
                r.target);
          },
      },
      p.data);
}

}  // namespace

auto LowerExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = unit_state.TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerPrimaryExpr(
                unit_state, scope_state, proc_state, p, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            auto operand_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(u.operand.value));
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const mir::ExprId operand_id =
                proc_scope_state.AddExpr(*std::move(operand_or));
            return mir::Expr{
                .data =
                    mir::UnaryExpr{
                        .op = LowerUnaryOp(u.op), .operand = operand_id},
                .type = result_type};
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            auto lhs_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.lhs.value));
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            const mir::ExprId lhs_id =
                proc_scope_state.AddExpr(*std::move(lhs_or));
            auto rhs_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.rhs.value));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id =
                proc_scope_state.AddExpr(*std::move(rhs_or));
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
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(a.rhs.value));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id =
                proc_scope_state.AddExpr(*std::move(rhs_or));
            return mir::Expr{
                .data =
                    mir::AssignExpr{
                        .target = LowerRefAsLvalue(
                            scope_state, proc_state, ref->get().target),
                        .value = rhs_id},
                .type = result_type};
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            auto operand_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(cv.operand.value));
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const mir::ExprId operand_id =
                proc_scope_state.AddExpr(*std::move(operand_or));
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
                          unit_state, scope_state, proc_state, proc_scope_state,
                          hir_process, c, sys, expr.span);
                    },
                    [&](const hir::StructuralSubroutineRef& usr)
                        -> diag::Result<mir::Expr> {
                      std::vector<mir::ExprId> args;
                      args.reserve(c.arguments.size());
                      for (const auto arg : c.arguments) {
                        auto arg_or = LowerExpr(
                            unit_state, scope_state, proc_state,
                            proc_scope_state, hir_process,
                            hir_process.exprs.at(arg.value));
                        if (!arg_or)
                          return std::unexpected(std::move(arg_or.error()));
                        args.push_back(
                            proc_scope_state.AddExpr(*std::move(arg_or)));
                      }
                      return mir::Expr{
                          .data =
                              mir::CallExpr{
                                  .callee = LowerUserCallee(scope_state, usr),
                                  .arguments = std::move(args)},
                          .type = result_type};
                    },
                },
                c.callee);
          },
      },
      expr.data);
}

namespace {

auto LowerExprImpl(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr,
    LoopVarLoweringMode loop_var_mode) -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = unit_state.TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerPrimaryExpr(
                unit_state, scope_state, ctor_state, p, result_type,
                loop_var_mode);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            auto operand_or = LowerExprImpl(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                scope.GetExpr(u.operand), loop_var_mode);
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const mir::ExprId operand_id =
                proc_scope_state.AddExpr(*std::move(operand_or));
            return mir::Expr{
                .data =
                    mir::UnaryExpr{
                        .op = LowerUnaryOp(u.op), .operand = operand_id},
                .type = result_type};
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            auto lhs_or = LowerExprImpl(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                scope.GetExpr(b.lhs), loop_var_mode);
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            const mir::ExprId lhs_id =
                proc_scope_state.AddExpr(*std::move(lhs_or));
            auto rhs_or = LowerExprImpl(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                scope.GetExpr(b.rhs), loop_var_mode);
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id =
                proc_scope_state.AddExpr(*std::move(rhs_or));
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
            auto rhs_or = LowerExprImpl(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                scope.GetExpr(a.rhs), loop_var_mode);
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            const mir::ExprId rhs_id =
                proc_scope_state.AddExpr(*std::move(rhs_or));
            return mir::Expr{
                .data =
                    mir::AssignExpr{
                        .target = LowerRefAsLvalue(
                            scope_state, ctor_state, ref->get().target,
                            loop_var_mode),
                        .value = rhs_id},
                .type = result_type};
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            auto operand_or = LowerExprImpl(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                scope.GetExpr(cv.operand), loop_var_mode);
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const mir::ExprId operand_id =
                proc_scope_state.AddExpr(*std::move(operand_or));
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

}  // namespace

auto LowerProceduralExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState& ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  return LowerExprImpl(
      unit_state, scope_state, &ctor_state, proc_scope_state, scope, expr,
      LoopVarLoweringMode::kProceduralInduction);
}

auto LowerStructuralExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  return LowerExprImpl(
      unit_state, scope_state, nullptr, proc_scope_state, scope, expr,
      LoopVarLoweringMode::kStructuralParam);
}

}  // namespace lyra::lowering::hir_to_mir
