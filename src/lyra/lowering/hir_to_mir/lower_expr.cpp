#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <cstdint>
#include <memory>
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

auto LowerHirLvalueProc(
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state, const hir::Lvalue& lvalue)
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
                "LowerHirLvalueProc: HIR LoopVarRef must not appear as an "
                "lvalue in a process body; the for-loop header is constructor-"
                "side, not procedural");
          },
      },
      lvalue);
}

auto LowerUserCallee(
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralSubroutineRef& u) -> mir::Callee {
  return scope_state.TranslateStructuralSubroutine(u.hops, u.subroutine);
}

auto LowerHirIntegerLiteral(const hir::IntegerLiteral& i, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data = mir::IntegerLiteral{.value = LowerHirIntegralConstant(i.value)},
      .type = type};
}

auto LowerBuiltinMethodCall(
    const UnitLoweringState& unit_state, const hir::Process& hir_process,
    hir::BuiltinMethodKind kind, const std::vector<hir::ExprId>& arguments,
    mir::TypeId result_type, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  if (arguments.empty()) {
    throw InternalError(
        "LowerBuiltinMethodCall: built-in method call has no receiver");
  }
  const hir::TypeId receiver_type_id =
      hir_process.exprs.at(arguments[0].value).type;
  const auto& receiver_type = unit_state.GetHirType(receiver_type_id);
  if (!receiver_type.IsEnum()) {
    throw InternalError(
        "LowerBuiltinMethodCall: enum builtin method on non-enum receiver");
  }
  const auto& enum_type = receiver_type.AsEnum();

  switch (kind) {
    case hir::BuiltinMethodKind::kEnumFirst:
    case hir::BuiltinMethodKind::kEnumLast: {
      if (enum_type.members.empty()) {
        throw InternalError("LowerBuiltinMethodCall: enum has no members");
      }
      const hir::EnumMember& picked =
          (kind == hir::BuiltinMethodKind::kEnumFirst)
              ? enum_type.members.front()
              : enum_type.members.back();
      return mir::Expr{
          .data =
              mir::IntegerLiteral{
                  .value = LowerHirIntegralConstant(picked.value)},
          .type = result_type};
    }
    case hir::BuiltinMethodKind::kEnumNum: {
      const auto count = static_cast<std::uint32_t>(enum_type.members.size());
      hir::IntegralConstant value{
          .value_words = {static_cast<std::uint64_t>(count)},
          .state_words = {},
          .width = 32U,
          .signedness = hir::Signedness::kSigned,
          .state_kind = hir::IntegralStateKind::kTwoState,
      };
      return mir::Expr{
          .data = mir::IntegerLiteral{.value = LowerHirIntegralConstant(value)},
          .type = result_type};
    }
    case hir::BuiltinMethodKind::kEnumNext:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "enum method 'next' is not yet supported",
          diag::UnsupportedCategory::kOperation);
    case hir::BuiltinMethodKind::kEnumPrev:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "enum method 'prev' is not yet supported",
          diag::UnsupportedCategory::kOperation);
    case hir::BuiltinMethodKind::kEnumName:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "enum method 'name' is not yet supported",
          diag::UnsupportedCategory::kOperation);
  }
  throw InternalError("LowerBuiltinMethodCall: unknown BuiltinMethodKind");
}

auto LowerHirStringLiteral(const hir::StringLiteral& s, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{.data = mir::StringLiteral{.value = s.value}, .type = type};
}

auto LowerHirTimeLiteral(
    const UnitLoweringState& unit_state, const hir::TimeLiteral& t)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::TimeLiteral{.value = t.value, .scale = LowerTimeScale(t.scale)},
      .type = unit_state.Builtins().realtime};
}

auto LowerHirPrimaryProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state, const hir::Primary& p,
    mir::TypeId type) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(s, type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(unit_state, t);
          },
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
      p);
}

auto LowerHirPrimaryStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state, const hir::Primary& p,
    mir::TypeId type, LoopVarLoweringMode loop_var_mode) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(s, type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(unit_state, t);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(scope_state, m, type);
          },
          [](const hir::ProceduralVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryStructural: HIR ProceduralVarRef does not "
                "appear in constructor expressions");
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            if (loop_var_mode == LoopVarLoweringMode::kProceduralInduction) {
              if (ctor_state == nullptr) {
                throw InternalError(
                    "LowerHirPrimaryStructural: kProceduralInduction mode "
                    "requires a non-null ConstructorLoweringState");
              }
              return LowerLoopVarRefExpr(*ctor_state, lv, type);
            }
            return LowerStructuralParamRefExpr(scope_state, lv, type);
          },
      },
      p);
}

// Builds a ClosureExpr that snapshots the RHS by value into the body and writes
// the snapshot to the structural target. The closure is the deferred-write
// vehicle the NBA region invokes. The returned Expr has type `void` since the
// closure value itself is consumed only by RuntimeSubmitNbaCall.
auto BuildNbaSubmitClosureExpr(
    const UnitLoweringState& unit_state, mir::Lvalue lhs_structural,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr {
  ProceduralScopeLoweringState body;
  const mir::ProceduralVarId snapshot_binding = body.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_nba_rhs", .type = rhs_type});
  const mir::ExprId snapshot_ref_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0},
                  .var = snapshot_binding},
          .type = rhs_type});
  const mir::ExprId assign_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{
                  .target = std::move(lhs_structural),
                  .value = snapshot_ref_id},
          .type = rhs_type});
  const mir::StmtId stmt_id = body.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = assign_id},
          .child_procedural_scopes = {}});
  body.AddRootStmt(stmt_id);

  mir::ClosureExpr closure;
  closure.captures.emplace_back(
      mir::ByValueCapture{
          .value = rhs_id_in_outer, .binding = snapshot_binding});
  closure.body = std::make_unique<mir::ProceduralScope>(body.Finish());

  return mir::Expr{
      .data = std::move(closure), .type = unit_state.Builtins().void_type};
}

auto LowerHirUnaryExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto operand_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(u.operand.value));
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data = mir::UnaryExpr{.op = LowerUnaryOp(u.op), .operand = operand_id},
      .type = result_type};
}

auto LowerHirBinaryExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto lhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(b.lhs.value));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(b.rhs.value));
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = proc_scope_state.AddExpr(*std::move(rhs_or));
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(b.op), .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

auto LowerHirConditionalExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(c.condition.value));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(c.then_value.value));
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = proc_scope_state.AddExpr(*std::move(then_or));
  auto else_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(c.else_value.value));
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = proc_scope_state.AddExpr(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirAssignExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto rhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(a.rhs.value));
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId rhs_type = (*rhs_or).type;
  const mir::ExprId rhs_id = proc_scope_state.AddExpr(*std::move(rhs_or));
  auto lhs = LowerHirLvalueProc(scope_state, proc_state, a.lhs);

  if (a.kind == hir::AssignKind::kBlocking) {
    return mir::Expr{
        .data = mir::AssignExpr{.target = lhs, .value = rhs_id},
        .type = result_type};
  }

  if (!std::holds_alternative<mir::StructuralVarRef>(lhs)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "non-blocking assignment to procedural local is not supported yet",
        diag::UnsupportedCategory::kFeature);
  }

  mir::Expr closure_expr =
      BuildNbaSubmitClosureExpr(unit_state, std::move(lhs), rhs_id, rhs_type);
  const mir::ExprId closure_id =
      proc_scope_state.AddExpr(std::move(closure_expr));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeSubmitNbaCall{.closure = closure_id}},
      .type = unit_state.Builtins().void_type};
}

auto LowerHirConversionExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto operand_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(cv.operand.value));
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data =
          mir::ConversionExpr{
              .operand = operand_id, .kind = LowerHirConversionKind(cv.kind)},
      .type = result_type};
}

auto LowerHirCallExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            return LowerSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, sys, span);
          },
          [&](const hir::StructuralSubroutineRef& usr)
              -> diag::Result<mir::Expr> {
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size());
            for (const auto arg : c.arguments) {
              auto arg_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_process, hir_process.exprs.at(arg.value));
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              args.push_back(proc_scope_state.AddExpr(*std::move(arg_or)));
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = LowerUserCallee(scope_state, usr),
                        .arguments = std::move(args)},
                .type = result_type};
          },
          [&](const hir::MethodRef& m) -> diag::Result<mir::Expr> {
            const auto& method =
                unit_state.GetHirType(m.receiver_type).GetMethod(m.method);
            return std::visit(
                Overloaded{
                    [&](const hir::BuiltinMethod& b)
                        -> diag::Result<mir::Expr> {
                      return LowerBuiltinMethodCall(
                          unit_state, hir_process, b.kind, c.arguments,
                          result_type, span);
                    },
                    [&](const hir::StructuralMethod&)
                        -> diag::Result<mir::Expr> {
                      throw InternalError(
                          "MethodRef -> StructuralMethod lowering: "
                          "user-defined methods are not yet wired into HIR/MIR "
                          "call dispatch");
                    },
                },
                method.data);
          },
      },
      c.callee);
}

// Builds the 1-bit MIR predicate ExprId for one inside-operator item, comparing
// against the already-lowered LHS. Value items use `==`; range items use
// `(>= lo) && (<= hi)`. The returned ExprId references a freshly added MIR
// expression in `proc_scope_state`.
auto BuildHirInsideItemPredicateProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, mir::ExprId lhs_id,
    const hir::InsideItem& item, mir::TypeId result_type)
    -> diag::Result<mir::ExprId> {
  auto lower_id = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        hir_process.exprs.at(id.value));
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    return proc_scope_state.AddExpr(*std::move(lowered));
  };

  return std::visit(
      Overloaded{
          [&](const hir::ExprId& val_id) -> diag::Result<mir::ExprId> {
            auto v = lower_id(val_id);
            if (!v) return std::unexpected(std::move(v.error()));
            return proc_scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kWildcardEquality,
                            .lhs = lhs_id,
                            .rhs = *v},
                    .type = result_type});
          },
          [&](const hir::InsideRangePair& r) -> diag::Result<mir::ExprId> {
            auto lo = lower_id(r.lo);
            if (!lo) return std::unexpected(std::move(lo.error()));
            auto hi = lower_id(r.hi);
            if (!hi) return std::unexpected(std::move(hi.error()));
            const mir::ExprId ge_id = proc_scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kGreaterEqual,
                            .lhs = lhs_id,
                            .rhs = *lo},
                    .type = result_type});
            const mir::ExprId le_id = proc_scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLessEqual,
                            .lhs = lhs_id,
                            .rhs = *hi},
                    .type = result_type});
            return proc_scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLogicalAnd,
                            .lhs = ge_id,
                            .rhs = le_id},
                    .type = result_type});
          },
      },
      item);
}

auto LowerHirInsideExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::InsideExpr& in,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto lhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(in.lhs.value));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));

  std::vector<mir::ExprId> predicates;
  predicates.reserve(in.items.size());
  for (const auto& item : in.items) {
    auto pred_or = BuildHirInsideItemPredicateProc(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        lhs_id, item, result_type);
    if (!pred_or) return std::unexpected(std::move(pred_or.error()));
    predicates.push_back(*pred_or);
  }

  if (predicates.empty()) {
    throw InternalError(
        "LowerHirInsideExprProc: hir::InsideExpr has empty item list");
  }
  if (predicates.size() == 1) {
    return mir::Expr{proc_scope_state.GetExpr(predicates.front())};
  }
  mir::ExprId acc = predicates.front();
  for (std::size_t i = 1; i + 1 < predicates.size(); ++i) {
    acc = proc_scope_state.AddExpr(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kLogicalOr,
                    .lhs = acc,
                    .rhs = predicates[i]},
            .type = result_type});
  }
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = mir::BinaryOp::kLogicalOr,
              .lhs = acc,
              .rhs = predicates.back()},
      .type = result_type};
}

auto LowerHirElementSelectExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto base_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(sel.base_value.value));
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));

  auto idx_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(sel.index.value));
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = proc_scope_state.AddExpr(*std::move(idx_or));

  return mir::Expr{
      .data =
          mir::ElementSelectExpr{
              .base_value = base_id,
              .index = idx_id,
          },
      .type = result_type};
}

auto LowerHirRangeSelectExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto base_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(sel.base_value.value));
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));

  auto bounds_or = std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b)
              -> diag::Result<mir::RangeBounds> {
            auto msb_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.msb_expr.value));
            if (!msb_or) return std::unexpected(std::move(msb_or.error()));
            const mir::ExprId msb_id =
                proc_scope_state.AddExpr(*std::move(msb_or));
            auto lsb_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.lsb_expr.value));
            if (!lsb_or) return std::unexpected(std::move(lsb_or.error()));
            const mir::ExprId lsb_id =
                proc_scope_state.AddExpr(*std::move(lsb_or));
            return mir::RangeConstantBounds{
                .msb_expr = msb_id, .lsb_expr = lsb_id};
          },
          [&](const hir::RangeIndexedUpBounds& b)
              -> diag::Result<mir::RangeBounds> {
            auto base_idx_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.base_index.value));
            if (!base_idx_or) {
              return std::unexpected(std::move(base_idx_or.error()));
            }
            const mir::ExprId base_idx_id =
                proc_scope_state.AddExpr(*std::move(base_idx_or));
            auto width_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.width.value));
            if (!width_or) return std::unexpected(std::move(width_or.error()));
            const mir::ExprId width_id =
                proc_scope_state.AddExpr(*std::move(width_or));
            return mir::RangeIndexedUpBounds{
                .base_index = base_idx_id, .width = width_id};
          },
          [&](const hir::RangeIndexedDownBounds& b)
              -> diag::Result<mir::RangeBounds> {
            auto base_idx_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.base_index.value));
            if (!base_idx_or) {
              return std::unexpected(std::move(base_idx_or.error()));
            }
            const mir::ExprId base_idx_id =
                proc_scope_state.AddExpr(*std::move(base_idx_or));
            auto width_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, hir_process.exprs.at(b.width.value));
            if (!width_or) return std::unexpected(std::move(width_or.error()));
            const mir::ExprId width_id =
                proc_scope_state.AddExpr(*std::move(width_or));
            return mir::RangeIndexedDownBounds{
                .base_index = base_idx_id, .width = width_id};
          },
      },
      sel.bounds);
  if (!bounds_or) return std::unexpected(std::move(bounds_or.error()));

  return mir::Expr{
      .data =
          mir::RangeSelectExpr{
              .base_value = base_id,
              .bounds = *std::move(bounds_or),
          },
      .type = result_type};
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
            return LowerHirPrimaryProc(
                unit_state, scope_state, proc_state, p.data, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, result_type);
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, a, expr.span, result_type);
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, cv, result_type);
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirCallExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, expr.span, result_type);
          },
          [&](const hir::InsideExpr& in) -> diag::Result<mir::Expr> {
            return LowerHirInsideExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, in, result_type);
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, sel, result_type);
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
    LoopVarLoweringMode loop_var_mode) -> diag::Result<mir::Expr>;

auto LowerHirUnaryExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::UnaryExpr& u,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto operand_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(u.operand), loop_var_mode);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data = mir::UnaryExpr{.op = LowerUnaryOp(u.op), .operand = operand_id},
      .type = result_type};
}

auto LowerHirBinaryExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::BinaryExpr& b,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto lhs_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(b.lhs), loop_var_mode);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(b.rhs), loop_var_mode);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = proc_scope_state.AddExpr(*std::move(rhs_or));
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(b.op), .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

auto LowerHirConditionalExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::ConditionalExpr& c,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto cond_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(c.condition), loop_var_mode);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(c.then_value), loop_var_mode);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = proc_scope_state.AddExpr(*std::move(then_or));
  auto else_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(c.else_value), loop_var_mode);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = proc_scope_state.AddExpr(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirConversionExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::ConversionExpr& cv,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
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
              .operand = operand_id, .kind = LowerHirConversionKind(cv.kind)},
      .type = result_type};
}

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
            return LowerHirPrimaryStructural(
                unit_state, scope_state, ctor_state, p.data, result_type,
                loop_var_mode);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, u,
                loop_var_mode, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, b,
                loop_var_mode, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, c,
                loop_var_mode, result_type);
          },
          [](const hir::AssignExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerExprImpl (structural): HIR AssignExpr does not appear "
                "in constructor-side expressions; structural code has no "
                "general assignment");
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                cv, loop_var_mode, result_type);
          },
          [](const hir::CallExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "calls are not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::InsideExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "inside operator is not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::ElementSelectExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "element-select in constructor expressions is not yet "
                "supported",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::RangeSelectExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "range-select in constructor expressions is not yet supported",
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
