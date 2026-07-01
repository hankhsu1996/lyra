#include "lyra/lowering/hir_to_mir/expression/operators.hpp"

#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

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

namespace {

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

auto LowerIncDecOp(hir::IncDecOp op) -> mir::IncDecOp {
  switch (op) {
    case hir::IncDecOp::kPreInc:
      return mir::IncDecOp::kPreInc;
    case hir::IncDecOp::kPostInc:
      return mir::IncDecOp::kPostInc;
    case hir::IncDecOp::kPreDec:
      return mir::IncDecOp::kPreDec;
    case hir::IncDecOp::kPostDec:
      return mir::IncDecOp::kPostDec;
  }
  throw InternalError("LowerIncDecOp: unknown HIR IncDecOp");
}

auto IsRealFamilyTypeKind(mir::TypeKind k) -> bool {
  return k == mir::TypeKind::kReal || k == mir::TypeKind::kShortReal ||
         k == mir::TypeKind::kRealTime;
}

auto IsRealFamilyType(const mir::Type& ty) -> bool {
  return IsRealFamilyTypeKind(ty.Kind());
}

auto IsArrayContainerType(const mir::Type& ty) -> bool {
  return std::holds_alternative<mir::UnpackedArrayType>(ty.data) ||
         std::holds_alternative<mir::DynamicArrayType>(ty.data);
}

// Maps the method-style MIR binary ops to their `BuiltinFn` realization on
// `PackedArray`. Returns `nullopt` for ops that render natively (`+`, `==`,
// ...). The negation-then-positive forms (`kWildcardInequality`,
// `kCaseInequality`) are handled separately by the caller as `!Eq(...)`.
auto BinaryOpAsBuiltinFn(mir::BinaryOp op)
    -> std::optional<support::BuiltinFn> {
  switch (op) {
    case mir::BinaryOp::kPower:
      return support::BuiltinFn::kPow;
    case mir::BinaryOp::kShiftLeft:
      return support::BuiltinFn::kShiftLeft;
    case mir::BinaryOp::kLogicalShiftRight:
      return support::BuiltinFn::kLogicalShiftRight;
    case mir::BinaryOp::kArithmeticShiftRight:
      return support::BuiltinFn::kArithmeticShiftRight;
    case mir::BinaryOp::kBitwiseXnor:
      return support::BuiltinFn::kBitwiseXnor;
    case mir::BinaryOp::kLogicalImplication:
      return support::BuiltinFn::kLogicalImplication;
    case mir::BinaryOp::kLogicalEquivalence:
      return support::BuiltinFn::kLogicalEquivalence;
    case mir::BinaryOp::kWildcardEquality:
      return support::BuiltinFn::kWildcardEquals;
    case mir::BinaryOp::kCaseEquality:
      return support::BuiltinFn::kCaseEqual;
    case mir::BinaryOp::kCasezEquality:
      return support::BuiltinFn::kCasezEquals;
    case mir::BinaryOp::kCasexEquality:
      return support::BuiltinFn::kCasexEquals;
    default:
      return std::nullopt;
  }
}

auto UnaryOpAsBuiltinFn(mir::UnaryOp op) -> std::optional<support::BuiltinFn> {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
      return support::BuiltinFn::kReductionAnd;
    case mir::UnaryOp::kReductionOr:
      return support::BuiltinFn::kReductionOr;
    case mir::UnaryOp::kReductionXor:
      return support::BuiltinFn::kReductionXor;
    case mir::UnaryOp::kReductionNand:
      return support::BuiltinFn::kReductionNand;
    case mir::UnaryOp::kReductionNor:
      return support::BuiltinFn::kReductionNor;
    case mir::UnaryOp::kReductionXnor:
      return support::BuiltinFn::kReductionXnor;
    default:
      return std::nullopt;
  }
}

auto MakeBuiltinFnCall(
    support::BuiltinFn id, std::vector<mir::ExprId> arguments,
    mir::TypeId result_type) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = id},
              .arguments = std::move(arguments)},
      .type = result_type};
}

auto MakeFromBoolCall(mir::ExprId bool_expr_id, mir::TypeId result_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFromBool,
                      .qualification = mir::TypeQualifier{.type = result_type}},
              .arguments = {bool_expr_id}},
      .type = result_type};
}

// Wraps an operand in `BoolCastExpr` so a host-bool consumer (a native
// `&&` / `||` / `!`, or `kFromBool`) can take it. The cast's MIR type is
// the unit's canonical 1-bit packed type because the wrap is observable
// only after `FromBool` re-shapes it; the bool-ness rides on the node kind,
// not on the type.
auto MakeBoolCast(mir::ExprId operand_id, mir::TypeId bit1_type) -> mir::Expr {
  return mir::Expr{
      .data = mir::BoolCastExpr{.operand = operand_id}, .type = bit1_type};
}

// LRM 11.3.1 logical operator on real / string operands: each operand
// passes through `bool(...)`, then the host-native logical operator
// composes them, then `kFromBool` re-shapes to a 1-bit integral.
auto BuildRealOrStringLogicalLift(
    mir::Block& block, mir::BinaryOp op, mir::ExprId lhs_id, mir::ExprId rhs_id,
    mir::TypeId result_type, mir::TypeId bit1_type) -> mir::Expr {
  const mir::ExprId lhs_bool = block.exprs.Add(MakeBoolCast(lhs_id, bit1_type));
  const mir::ExprId rhs_bool = block.exprs.Add(MakeBoolCast(rhs_id, bit1_type));
  mir::ExprId inner{};
  switch (op) {
    case mir::BinaryOp::kLogicalAnd:
    case mir::BinaryOp::kLogicalOr:
    case mir::BinaryOp::kLogicalEquivalence: {
      inner = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = op == mir::BinaryOp::kLogicalEquivalence
                                ? mir::BinaryOp::kEquality
                                : op,
                      .lhs = lhs_bool,
                      .rhs = rhs_bool},
              .type = result_type});
      break;
    }
    case mir::BinaryOp::kLogicalImplication: {
      // `!lhs || rhs`
      const mir::ExprId not_lhs = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::UnaryExpr{
                      .op = mir::UnaryOp::kLogicalNot, .operand = lhs_bool},
              .type = result_type});
      inner = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kLogicalOr,
                      .lhs = not_lhs,
                      .rhs = rhs_bool},
              .type = result_type});
      break;
    }
    default:
      throw InternalError(
          "BuildRealOrStringLogicalLift: unsupported logical operator");
  }
  return MakeFromBoolCall(inner, result_type);
}

}  // namespace

auto BuildMirUnaryExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::UnaryOp op,
    mir::ExprId operand_id, mir::TypeId result_type) -> mir::Expr {
  const auto& operand_ty = unit.types.Get(block.exprs.Get(operand_id).type);

  // LRM 11.4.9 reduction operators: render as `PackedArray::ReductionX()`,
  // routed through a `Direct` builtin call so the backend's render path
  // collapses to mechanical method dispatch.
  if (auto builtin = UnaryOpAsBuiltinFn(op)) {
    return MakeBuiltinFnCall(*builtin, {operand_id}, result_type);
  }

  // LRM 11.3.1 real `!`: route through `bool(...)` and wrap the host-bool
  // result in `FromBool` so the surface type stays the 1-bit integral the
  // SV semantic prescribes.
  if (op == mir::UnaryOp::kLogicalNot && IsRealFamilyType(operand_ty)) {
    const mir::ExprId operand_bool =
        block.exprs.Add(MakeBoolCast(operand_id, unit.builtins.bit1));
    const mir::ExprId not_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::UnaryExpr{
                    .op = mir::UnaryOp::kLogicalNot, .operand = operand_bool},
            .type = result_type});
    return MakeFromBoolCall(not_id, result_type);
  }

  return mir::Expr{
      .data = mir::UnaryExpr{.op = op, .operand = operand_id},
      .type = result_type};
}

auto BuildMirBinaryExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::BinaryOp op,
    mir::ExprId lhs_id, mir::ExprId rhs_id, mir::TypeId result_type)
    -> mir::Expr {
  const auto& lhs_ty = unit.types.Get(block.exprs.Get(lhs_id).type);
  const auto& rhs_ty = unit.types.Get(block.exprs.Get(rhs_id).type);
  const bool real_lhs = IsRealFamilyType(lhs_ty);
  const bool real_rhs = IsRealFamilyType(rhs_ty);
  const bool string_lhs = lhs_ty.Kind() == mir::TypeKind::kString;
  const bool string_rhs = rhs_ty.Kind() == mir::TypeKind::kString;

  // LRM 8.4: handle equality compares object identity and yields a 1-bit
  // value. A handle's `==` / `!=` produces a host bool, reshaped to the SV
  // 1-bit integral by `FromBool` so the result carries the value type a 1-bit
  // signal assignment expects.
  const auto is_handle = [](const mir::Type& ty) {
    return ty.Kind() == mir::TypeKind::kManagedRef ||
           ty.Kind() == mir::TypeKind::kChandle;
  };
  if ((is_handle(lhs_ty) || is_handle(rhs_ty)) &&
      (op == mir::BinaryOp::kEquality || op == mir::BinaryOp::kInequality)) {
    const mir::ExprId cmp = block.exprs.Add(
        mir::Expr{
            .data = mir::BinaryExpr{.op = op, .lhs = lhs_id, .rhs = rhs_id},
            .type = result_type});
    return MakeFromBoolCall(cmp, result_type);
  }

  // LRM 11.3.1 / LRM 6.16 logical operator on real / string operands needs
  // `bool(...)` coercion on each operand before the host-native `&&` / `||`
  // / `==` composes them; `kFromBool` re-shapes the host bool back to a
  // 1-bit integral. Comparison / relational operators do NOT need this
  // lift -- `String::operator==` / `RealValue::operator==` and their
  // siblings already return a SV `PackedArray<1>` directly per LRM 6.16 /
  // 11.3.1, so the plain BinaryExpr render does the right thing.
  if (real_lhs || real_rhs || (string_lhs && string_rhs)) {
    switch (op) {
      case mir::BinaryOp::kLogicalAnd:
      case mir::BinaryOp::kLogicalOr:
      case mir::BinaryOp::kLogicalImplication:
      case mir::BinaryOp::kLogicalEquivalence:
        return BuildRealOrStringLogicalLift(
            block, op, lhs_id, rhs_id, result_type, unit.builtins.bit1);
      default:
        break;
    }
  }

  // LRM 11.2.2 / 11.4.5 aggregate `===` / `!==`: routed as `kCaseEqual`
  // (with a wrapping `!` for the inequality form).
  if (IsArrayContainerType(lhs_ty) && IsArrayContainerType(rhs_ty)) {
    if (op == mir::BinaryOp::kCaseEquality) {
      return MakeBuiltinFnCall(
          support::BuiltinFn::kCaseEqual, {lhs_id, rhs_id}, result_type);
    }
    if (op == mir::BinaryOp::kCaseInequality) {
      const mir::ExprId inner = block.exprs.Add(MakeBuiltinFnCall(
          support::BuiltinFn::kCaseEqual, {lhs_id, rhs_id}, result_type));
      return mir::Expr{
          .data =
              mir::UnaryExpr{.op = mir::UnaryOp::kLogicalNot, .operand = inner},
          .type = result_type};
    }
    // kEquality / kInequality on arrays render natively (operator==),
    // so fall through to the BinaryExpr path.
  }

  // LRM 11.4 method-style binary ops (shifts, power, xnor, wildcard /
  // case / implication / equivalence): route through a `Direct` builtin call.
  if (auto builtin = BinaryOpAsBuiltinFn(op)) {
    return MakeBuiltinFnCall(*builtin, {lhs_id, rhs_id}, result_type);
  }

  // The `!=` siblings of wildcard / case equality lift to a positive form
  // wrapped in logical NOT, keeping the backend free of negation knowledge.
  if (op == mir::BinaryOp::kWildcardInequality) {
    const mir::ExprId inner = block.exprs.Add(MakeBuiltinFnCall(
        support::BuiltinFn::kWildcardEquals, {lhs_id, rhs_id}, result_type));
    return mir::Expr{
        .data =
            mir::UnaryExpr{.op = mir::UnaryOp::kLogicalNot, .operand = inner},
        .type = result_type};
  }
  if (op == mir::BinaryOp::kCaseInequality) {
    const mir::ExprId inner = block.exprs.Add(MakeBuiltinFnCall(
        support::BuiltinFn::kCaseEqual, {lhs_id, rhs_id}, result_type));
    return mir::Expr{
        .data =
            mir::UnaryExpr{.op = mir::UnaryOp::kLogicalNot, .operand = inner},
        .type = result_type};
  }

  return mir::Expr{
      .data = mir::BinaryExpr{.op = op, .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

template <ExprLowerer Lowerer>
auto LowerHirUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto operand_or = lowerer.LowerExpr(lowerer.HirExprs().Get(u.operand), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id = block.exprs.Add(*std::move(operand_or));
  return BuildMirUnaryExpr(
      lowerer.Module().Unit(), block, LowerUnaryOp(u.op), operand_id,
      result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirBinaryExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto lhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(b.lhs), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = block.exprs.Add(*std::move(lhs_or));
  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(b.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = block.exprs.Add(*std::move(rhs_or));
  return BuildMirBinaryExpr(
      lowerer.Module().Unit(), block, LowerBinaryOp(b.op), lhs_id, rhs_id,
      result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirConditionalExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto cond_or = lowerer.LowerExpr(lowerer.HirExprs().Get(c.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = block.exprs.Add(*std::move(cond_or));
  auto then_or = lowerer.LowerExpr(lowerer.HirExprs().Get(c.then_value), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = block.exprs.Add(*std::move(then_or));
  auto else_or = lowerer.LowerExpr(lowerer.HirExprs().Get(c.else_value), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = block.exprs.Add(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirIncDecExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  // The target is written in place, so a queue element dispatches to its
  // write-side access just as an assignment target does.
  auto target_or = process.LowerLhsExpr(
      process.HirExprs().Get(inc.target), frame.WithLvalueTarget(true));
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  mir::ExprId target_id = block.exprs.Add(*std::move(target_or));

  // If the LHS reaches an observable storage cell, the mutation runs inside
  // a `ScopedMutation` snapshot so subscribers fire once on destructor commit.
  const mir::ExprId root_id =
      FindLhsRootId(process.Module().Unit(), block, target_id);
  if (mir::IsObservableCellType(
          process.Module().Unit().types.Get(block.exprs.Get(root_id).type))) {
    const mir::ExprId services_id =
        block.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
    target_id = RewriteLhsRootWithMutate(
        process.Module().Unit(), block, target_id, services_id);
  }

  return mir::Expr{
      .data = mir::IncDecExpr{.op = LowerIncDecOp(inc.op), .target = target_id},
      .type = result_type};
}

template <ExprLowerer Lowerer>
auto LowerHirConversionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto operand_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(cv.operand), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      frame.current_block->exprs.Add(*std::move(operand_or));
  return BuildValueConversion(
      lowerer.Module().Unit(), *frame.current_block, operand_id, result_type);
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private,
// so the dispatchers in process_lowerer.cpp / structural_scope_lowerer.cpp link
// against the symbols emitted here.
template auto LowerHirUnaryExpr(
    ProcessLowerer&, WalkFrame, const hir::UnaryExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirUnaryExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::UnaryExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirBinaryExpr(
    ProcessLowerer&, WalkFrame, const hir::BinaryExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirBinaryExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::BinaryExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirConditionalExpr(
    ProcessLowerer&, WalkFrame, const hir::ConditionalExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirConditionalExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ConditionalExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirConversionExpr(
    ProcessLowerer&, WalkFrame, const hir::ConversionExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirConversionExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ConversionExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
