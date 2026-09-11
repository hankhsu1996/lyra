#include "lyra/lowering/hir_to_mir/expression/operators.hpp"

#include <algorithm>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
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
    case hir::BinaryOp::kBitwiseAnd:
      return mir::BinaryOp::kBitwiseAnd;
    case hir::BinaryOp::kBitwiseOr:
      return mir::BinaryOp::kBitwiseOr;
    case hir::BinaryOp::kBitwiseXor:
      return mir::BinaryOp::kBitwiseXor;
    case hir::BinaryOp::kEquality:
      return mir::BinaryOp::kEquality;
    case hir::BinaryOp::kInequality:
      return mir::BinaryOp::kInequality;
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
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
    case hir::BinaryOp::kLogicalShiftRight:
    case hir::BinaryOp::kArithmeticShiftRight:
    case hir::BinaryOp::kPower:
    case hir::BinaryOp::kBitwiseXnor:
    case hir::BinaryOp::kCaseEquality:
    case hir::BinaryOp::kCaseInequality:
    case hir::BinaryOp::kWildcardEquality:
    case hir::BinaryOp::kWildcardInequality:
    case hir::BinaryOp::kLogicalImplication:
    case hir::BinaryOp::kLogicalEquivalence:
      break;
  }
  throw InternalError(
      "LowerBinaryOp: the operator is performed by a library entry and is "
      "settled before a binary node is built");
}

auto LowerCompoundOperation(hir::BinaryOp op) -> CompoundOperation {
  switch (op) {
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return support::BuiltinFn::kShiftLeftAssign;
    case hir::BinaryOp::kLogicalShiftRight:
      return support::BuiltinFn::kLogicalShiftRightAssign;
    case hir::BinaryOp::kArithmeticShiftRight:
      return support::BuiltinFn::kArithmeticShiftRightAssign;
    case hir::BinaryOp::kAdd:
    case hir::BinaryOp::kSub:
    case hir::BinaryOp::kMul:
    case hir::BinaryOp::kDiv:
    case hir::BinaryOp::kMod:
    case hir::BinaryOp::kBitwiseAnd:
    case hir::BinaryOp::kBitwiseOr:
    case hir::BinaryOp::kBitwiseXor:
      return LowerBinaryOp(op);
    case hir::BinaryOp::kEquality:
    case hir::BinaryOp::kInequality:
    case hir::BinaryOp::kGreaterEqual:
    case hir::BinaryOp::kGreaterThan:
    case hir::BinaryOp::kLessEqual:
    case hir::BinaryOp::kLessThan:
    case hir::BinaryOp::kLogicalAnd:
    case hir::BinaryOp::kLogicalOr:
    case hir::BinaryOp::kPower:
    case hir::BinaryOp::kBitwiseXnor:
    case hir::BinaryOp::kCaseEquality:
    case hir::BinaryOp::kCaseInequality:
    case hir::BinaryOp::kWildcardEquality:
    case hir::BinaryOp::kWildcardInequality:
    case hir::BinaryOp::kLogicalImplication:
    case hir::BinaryOp::kLogicalEquivalence:
      break;
  }
  throw InternalError(
      "LowerCompoundOperation: the operator has no `op=` form (LRM 11.4.1) and "
      "reaches no assignment");
}

auto BuildMirLogicalAnd(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId bit1_type,
    std::span<const mir::ExprId> tests) -> mir::ExprId {
  if (tests.empty()) {
    return BuildBit1Literal(unit, block, true);
  }
  mir::ExprId acc = tests.front();
  for (const mir::ExprId test : tests.subspan(1)) {
    acc = block.exprs.Add(BuildMirBinaryExpr(
        unit, block, hir::BinaryOp::kLogicalAnd, acc, test, bit1_type));
  }
  return acc;
}

auto BuildMirLogicalOr(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId bit1_type,
    std::span<const mir::ExprId> tests) -> mir::ExprId {
  if (tests.empty()) {
    return BuildBit1Literal(unit, block, false);
  }
  mir::ExprId acc = tests.front();
  for (const mir::ExprId test : tests.subspan(1)) {
    acc = block.exprs.Add(BuildMirBinaryExpr(
        unit, block, hir::BinaryOp::kLogicalOr, acc, test, bit1_type));
  }
  return acc;
}

namespace {

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

// The operator a target language applies to the operand's value. The other
// source-level unary operators do not name one: a reduction is an operation
// over the operand's bits and unary plus changes nothing, so both are settled
// before a node is built.
auto ValueOperator(hir::UnaryOp op) -> mir::UnaryOp {
  switch (op) {
    case hir::UnaryOp::kMinus:
      return mir::UnaryOp::kMinus;
    case hir::UnaryOp::kBitwiseNot:
      return mir::UnaryOp::kBitwiseNot;
    case hir::UnaryOp::kLogicalNot:
      return mir::UnaryOp::kLogicalNot;
    case hir::UnaryOp::kPlus:
    case hir::UnaryOp::kReductionAnd:
    case hir::UnaryOp::kReductionOr:
    case hir::UnaryOp::kReductionXor:
    case hir::UnaryOp::kReductionNand:
    case hir::UnaryOp::kReductionNor:
    case hir::UnaryOp::kReductionXnor:
      break;
  }
  throw InternalError(
      "ValueOperator: the operator does not apply to a value and is settled "
      "before a unary node is built");
}

// The entry that performs a binary operator, for an operator a target cannot
// apply to two values of one type. Three reasons put one here: its operands are
// not two values of one type (a shift's amount is sized on its own, LRM
// 11.4.10); it reads a value's representation rather than its value (the case
// and wildcard equalities compare x and z as themselves, LRM 11.4.5 / 11.4.6);
// or it composes several operations into one (power, xnor, implication,
// equivalence).
auto BinaryLibraryEntry(hir::BinaryOp op) -> std::optional<support::BuiltinFn> {
  switch (op) {
    case hir::BinaryOp::kPower:
      return support::BuiltinFn::kPow;
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return support::BuiltinFn::kShiftLeft;
    case hir::BinaryOp::kLogicalShiftRight:
      return support::BuiltinFn::kLogicalShiftRight;
    case hir::BinaryOp::kArithmeticShiftRight:
      return support::BuiltinFn::kArithmeticShiftRight;
    case hir::BinaryOp::kBitwiseXnor:
      return support::BuiltinFn::kBitwiseXnor;
    case hir::BinaryOp::kLogicalImplication:
      return support::BuiltinFn::kLogicalImplication;
    case hir::BinaryOp::kLogicalEquivalence:
      return support::BuiltinFn::kLogicalEquivalence;
    case hir::BinaryOp::kWildcardEquality:
      return support::BuiltinFn::kWildcardEquals;
    // The case family answers at the width the clause fixes rather than at the
    // one the context asked for, so it is built where that width is stated. A
    // wildcard `!=` answers with the negation of the comparison beside it, so
    // the negation is what names it.
    case hir::BinaryOp::kCaseEquality:
    case hir::BinaryOp::kCaseInequality:
    case hir::BinaryOp::kWildcardInequality:
      return std::nullopt;
    case hir::BinaryOp::kAdd:
    case hir::BinaryOp::kSub:
    case hir::BinaryOp::kMul:
    case hir::BinaryOp::kDiv:
    case hir::BinaryOp::kMod:
    case hir::BinaryOp::kBitwiseAnd:
    case hir::BinaryOp::kBitwiseOr:
    case hir::BinaryOp::kBitwiseXor:
    case hir::BinaryOp::kEquality:
    case hir::BinaryOp::kInequality:
    case hir::BinaryOp::kGreaterEqual:
    case hir::BinaryOp::kGreaterThan:
    case hir::BinaryOp::kLessEqual:
    case hir::BinaryOp::kLessThan:
    case hir::BinaryOp::kLogicalAnd:
    case hir::BinaryOp::kLogicalOr:
      return std::nullopt;
  }
  throw InternalError("BinaryLibraryEntry: unknown HIR BinaryOp");
}

// The entry that performs a reduction (LRM 11.4.9). A reduction's meaning is
// an operation over the operand's bits, which every peer language reaches
// through a library call and none spells as an operator on a value.
auto ReductionBuiltinFn(hir::UnaryOp op) -> std::optional<support::BuiltinFn> {
  switch (op) {
    case hir::UnaryOp::kReductionAnd:
      return support::BuiltinFn::kReductionAnd;
    case hir::UnaryOp::kReductionOr:
      return support::BuiltinFn::kReductionOr;
    case hir::UnaryOp::kReductionXor:
      return support::BuiltinFn::kReductionXor;
    case hir::UnaryOp::kReductionNand:
      return support::BuiltinFn::kReductionNand;
    case hir::UnaryOp::kReductionNor:
      return support::BuiltinFn::kReductionNor;
    case hir::UnaryOp::kReductionXnor:
      return support::BuiltinFn::kReductionXnor;
    case hir::UnaryOp::kPlus:
    case hir::UnaryOp::kMinus:
    case hir::UnaryOp::kBitwiseNot:
    case hir::UnaryOp::kLogicalNot:
      return std::nullopt;
  }
  throw InternalError("ReductionBuiltinFn: unknown HIR UnaryOp");
}

auto MakeBuiltinFnCall(
    support::BuiltinFn id, mir::ExprId receiver,
    std::vector<mir::ExprId> arguments, mir::TypeId result_type) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = id, .receiver = receiver},
              .arguments = std::move(arguments)},
      .type = result_type};
}

auto MakeFromBoolCall(mir::ExprId bool_expr_id, mir::TypeId result_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFromBool},
              .arguments = {bool_expr_id}},
      .type = result_type};
}

// Reads an operand as a machine boolean, so a consumer that takes one -- a
// native `&&` / `||` / `!`, or `kFromBool` -- can be given it.
auto MakeBoolCast(const mir::CompilationUnit& unit, mir::ExprId operand_id)
    -> mir::Expr {
  return mir::Expr{
      .data = mir::CastExpr{.operand = operand_id},
      .type = unit.builtins.machine_bool};
}

// LRM 11.3.1 logical operator on real / string operands: each operand
// passes through `bool(...)`, then the host-native logical operator
// composes them, then `kFromBool` re-shapes to a 1-bit integral.
auto BuildRealOrStringLogicalLift(
    const mir::CompilationUnit& unit, mir::Block& block, hir::BinaryOp op,
    mir::ExprId lhs_id, mir::ExprId rhs_id, mir::TypeId result_type)
    -> mir::Expr {
  const mir::ExprId lhs_bool = block.exprs.Add(MakeBoolCast(unit, lhs_id));
  const mir::ExprId rhs_bool = block.exprs.Add(MakeBoolCast(unit, rhs_id));
  mir::ExprId inner{};
  switch (op) {
    case hir::BinaryOp::kLogicalAnd:
    case hir::BinaryOp::kLogicalOr:
    case hir::BinaryOp::kLogicalEquivalence: {
      // Equivalence over two predicates is their equality, which is what the
      // host operator composing them already is.
      inner = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = op == hir::BinaryOp::kLogicalEquivalence
                                ? mir::BinaryOp::kEquality
                                : LowerBinaryOp(op),
                      .lhs = lhs_bool,
                      .rhs = rhs_bool},
              .type = result_type});
      break;
    }
    case hir::BinaryOp::kLogicalImplication: {
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

// The MIR a source-level unary operator names. The operand arrives as an
// expression rather than an id because an operator that names nothing answers
// with it, and interning it first would leave a node nothing reaches.
auto BuildMirUnaryExpr(
    const mir::CompilationUnit& unit, mir::Block& block, hir::UnaryOp op,
    mir::Expr operand, mir::TypeId result_type) -> mir::Expr {
  const mir::Type& operand_ty = unit.types.Get(operand.type);

  // Unary plus leaves its operand's value unchanged (LRM 11.4.3), so the
  // expression it names is the operand itself.
  if (op == hir::UnaryOp::kPlus) {
    return operand;
  }

  const mir::ExprId operand_id = block.exprs.Add(std::move(operand));

  if (auto builtin = ReductionBuiltinFn(op)) {
    return MakeBuiltinFnCall(*builtin, operand_id, {}, result_type);
  }

  // LRM 11.3.1 real `!` and LRM 6.14 chandle `!`: route through `bool(...)` and
  // wrap the host-bool result in `FromBool` so the surface type stays the 1-bit
  // integral the SV semantic prescribes. A chandle's boolean value is 0 when it
  // is null and 1 otherwise.
  if (op == hir::UnaryOp::kLogicalNot &&
      (operand_ty.IsRealFamily() || operand_ty.Is<mir::ChandleType>())) {
    const mir::ExprId operand_bool =
        block.exprs.Add(MakeBoolCast(unit, operand_id));
    const mir::ExprId not_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::UnaryExpr{
                    .op = mir::UnaryOp::kLogicalNot, .operand = operand_bool},
            .type = result_type});
    return MakeFromBoolCall(not_id, result_type);
  }

  return mir::Expr{
      .data = mir::UnaryExpr{.op = ValueOperator(op), .operand = operand_id},
      .type = result_type};
}

}  // namespace

auto BuildMirBinaryExpr(
    mir::CompilationUnit& unit, mir::Block& block, hir::BinaryOp op,
    mir::ExprId lhs_id, mir::ExprId rhs_id, mir::TypeId result_type)
    -> mir::Expr {
  const auto& lhs_ty = unit.types.Get(block.exprs.Get(lhs_id).type);
  const auto& rhs_ty = unit.types.Get(block.exprs.Get(rhs_id).type);
  const bool real_lhs = lhs_ty.IsRealFamily();
  const bool real_rhs = rhs_ty.IsRealFamily();
  const bool string_lhs = lhs_ty.Is<mir::StringType>();
  const bool string_rhs = rhs_ty.Is<mir::StringType>();

  // LRM 8.4: class-handle equality asks which object each handle names, which
  // is a machine predicate; the 1-bit value LRM 11.4.5 gives the operator is
  // that predicate widened, so the two are stated separately -- the same shape
  // a real- or string-family logical operator takes. A chandle is compared as
  // the value it is (LRM 6.14) and needs neither step.
  const auto is_handle = [](const mir::Type& ty) {
    return ty.Is<mir::ManagedRefType>();
  };
  if ((is_handle(lhs_ty) || is_handle(rhs_ty)) &&
      (op == hir::BinaryOp::kEquality || op == hir::BinaryOp::kInequality)) {
    // LRM 8.4 admits `null` as one operand. A comparison states no destination,
    // so the front end leaves such an operand at the null type and the handle's
    // type is what it is being compared at; converting says that once, in the
    // one place a value crossing to another type is materialized.
    const mir::TypeId handle_type = is_handle(lhs_ty)
                                        ? block.exprs.Get(lhs_id).type
                                        : block.exprs.Get(rhs_id).type;
    const auto at_handle_type = [&](mir::ExprId operand) -> mir::ExprId {
      return ConvertToType(unit, block, operand, handle_type);
    };
    // The comparison is typed at the host bool it produces; the 1-bit result is
    // what the widening around it yields.
    const mir::ExprId cmp = block.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = LowerBinaryOp(op),
                    .lhs = at_handle_type(lhs_id),
                    .rhs = at_handle_type(rhs_id)},
            .type = unit.builtins.machine_bool});
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
      case hir::BinaryOp::kLogicalAnd:
      case hir::BinaryOp::kLogicalOr:
      case hir::BinaryOp::kLogicalImplication:
      case hir::BinaryOp::kLogicalEquivalence:
        return BuildRealOrStringLogicalLift(
            unit, block, op, lhs_id, rhs_id, result_type);
      default:
        break;
    }
  }

  // LRM 11.4.5 `===` / `!==` over any data type, including an aggregate, with a
  // wrapping `!` for the inequality form. The clause makes the result always a
  // known 1'b0 or 1'b1, so the entry answers with a two-state bit whatever its
  // operands carry; the call is stated at that, and a context wanting another
  // representation takes the conversion.
  if (op == hir::BinaryOp::kCaseEquality ||
      op == hir::BinaryOp::kCaseInequality) {
    const mir::TypeId known = unit.builtins.bit1;
    mir::ExprId answer = block.exprs.Add(MakeBuiltinFnCall(
        support::BuiltinFn::kCaseEqual, lhs_id, {rhs_id}, known));
    if (op == hir::BinaryOp::kCaseInequality) {
      answer = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::UnaryExpr{
                      .op = mir::UnaryOp::kLogicalNot, .operand = answer},
              .type = known});
    }
    return block.exprs.Get(ConvertToType(unit, block, answer, result_type));
  }

  if (auto builtin = BinaryLibraryEntry(op)) {
    return MakeBuiltinFnCall(*builtin, lhs_id, {rhs_id}, result_type);
  }

  // SV spells `!=?` as an operator of its own, answering with the negation of
  // the comparison beside it (LRM 11.4.6). Negating here leaves every consumer
  // free of negation knowledge. Unlike case inequality it takes no conversion:
  // the clause lets the answer be unknown, so it carries the state class its
  // operands do, which is the one the context asked for.
  if (op == hir::BinaryOp::kWildcardInequality) {
    const mir::ExprId inner = block.exprs.Add(MakeBuiltinFnCall(
        support::BuiltinFn::kWildcardEquals, lhs_id, {rhs_id}, result_type));
    return mir::Expr{
        .data =
            mir::UnaryExpr{.op = mir::UnaryOp::kLogicalNot, .operand = inner},
        .type = result_type};
  }
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(op), .lhs = lhs_id, .rhs = rhs_id},
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
  return BuildMirUnaryExpr(
      lowerer.Owner().Unit(), block, u.op, *std::move(operand_or), result_type);
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
      lowerer.Owner().Unit(), block, b.op, lhs_id, rhs_id, result_type);
}

// One arm of a conditional, at the type the conditional yields. Both arms carry
// that type whatever their own expressions produced (LRM 11.4.11), because what
// reads a conditional reads the type off the node: an arm left at its own hands
// a consumer a value shaped like something the node never claimed to be, on
// exactly the runs where that arm is taken.
template <ExprLowerer Lowerer>
auto BuildConditionalArm(
    Lowerer& lowerer, const WalkFrame& frame, hir::ExprId value,
    mir::TypeId result_type) -> diag::Result<mir::ExprId> {
  auto value_or = lowerer.LowerExpr(lowerer.HirExprs().Get(value), frame);
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  mir::Block& block = *frame.current_block;
  return ConvertToType(
      lowerer.Owner().Unit(), block, block.exprs.Add(*std::move(value_or)),
      result_type);
}

// LRM 11.4.11 over a predicate whose truth is three-valued: a predicate that
// settles selects one arm and the other is never evaluated; an ambiguous one
// selects neither, so both are evaluated and their results are combined bit by
// bit. Three outcomes over one predicate, which is a chain of two selections
// over the two ways it can settle -- so the operator states itself in the
// primitives every selection already uses, and no consumer is left to invent a
// way to evaluate an arm conditionally.
//
// The predicate is bound to a local because both selections read it and it is
// evaluated once. Each arm is named by the outcomes that need it, and reaches
// at most one of them on any run.
template <ExprLowerer Lowerer>
auto BuildMergingConditional(
    Lowerer& lowerer, const WalkFrame& frame, const hir::ConditionalExpr& c,
    mir::TypeId predicate_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& unit = lowerer.Owner().Unit();

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto predicate_or = lowerer.LowerExpr(
      lowerer.HirExprs().Get(c.conditions.front().expr), steps.Frame());
  if (!predicate_or) return std::unexpected(std::move(predicate_or.error()));
  const mir::ExprId predicate_value = body.exprs.Add(*std::move(predicate_or));
  const mir::LocalId predicate_var =
      steps.Bindings().DeclareAnonymous(predicate_type);
  body.AppendStmt(
      mir::LocalDeclStmt{.target = predicate_var, .init = predicate_value});
  const auto read_predicate = [&] {
    return body.exprs.Add(mir::MakeLocalRefExpr(predicate_var, predicate_type));
  };

  auto then_or =
      BuildConditionalArm(lowerer, steps.Frame(), c.then_value, result_type);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = *then_or;
  auto else_or =
      BuildConditionalArm(lowerer, steps.Frame(), c.else_value, result_type);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = *else_or;

  const mir::ExprId negated = body.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = read_predicate()},
          .type = predicate_type});
  // LRM 11.4.11 combines the two results bit by bit: a bit both arms know and
  // agree on survives and every other becomes x (Table 11-20). A value with no
  // parts that can agree that way answers with the Table 7-1 default of its own
  // type instead, and which of the two it is follows from the result type, so
  // it is settled here rather than left for a backend to work out.
  const mir::Type& result_ty = unit.types.Get(result_type);
  const bool combines_bitwise =
      result_ty.IsIntegralPacked() || result_ty.Is<mir::UnpackedArrayType>();
  const mir::ExprId combined =
      combines_bitwise
          ? body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        support::BuiltinFn::kMergeConditional,
                                    .receiver = then_id},
                            .arguments = {else_id}},
                    .type = result_type})
          : body.exprs.Add(BuildDefaultValueExpr(unit, body, result_type));
  const mir::ExprId else_or_combined = body.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = ReduceToCondition(unit, body, negated),
                  .then_value = else_id,
                  .else_value = combined},
          .type = result_type});
  const mir::ExprId selected = body.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = ReduceToCondition(unit, body, read_predicate()),
                  .then_value = then_id,
                  .else_value = else_or_combined},
          .type = result_type});
  return steps.Build(selected);
}

template <ExprLowerer Lowerer>
auto LowerHirConditionalExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto& unit = lowerer.Owner().Unit();
  const mir::TypeId bit1_type = unit.builtins.bit1;

  if (c.conditions.empty()) {
    throw InternalError(
        "LowerHirConditionalExpr: hir::ConditionalExpr has no clauses");
  }

  // LRM 11.4.11 reads the predicate's truth as three-valued, but only a
  // four-state predicate can reach the third answer, where neither arm is
  // selected and the two results are combined instead. Conjoining several
  // clauses yields a two-state bit whatever they were, so only a lone clause
  // can carry the third answer. The predicate's type settles which operator
  // this is, and a type is read without evaluating anything.
  if (c.conditions.size() == 1) {
    const mir::TypeId predicate_type = lowerer.Owner().TranslateType(
        lowerer.HirExprs().Get(c.conditions.front().expr).type);
    const mir::Type& predicate_ty = unit.types.Get(predicate_type);
    if (predicate_ty.IsIntegralPacked() &&
        predicate_ty.PackedShape().state_kind ==
            mir::IntegralStateKind::kFourState) {
      return BuildMergingConditional(
          lowerer, frame, c, predicate_type, result_type);
    }
  }

  // Every clause's predicate is conjoined left to right; `&&` short-circuits,
  // so a clause is evaluated only when the ones before it held (LRM 12.6.3).
  // A clause carrying no pattern contributes its own expression, which is the
  // whole predicate for a plain `cond ? a : b`.
  std::vector<mir::ExprId> clauses;
  clauses.reserve(c.conditions.size());
  for (const hir::ConditionClause& clause : c.conditions) {
    auto clause_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(clause.expr), frame);
    if (!clause_or) return std::unexpected(std::move(clause_or.error()));
    clauses.push_back(block.exprs.Add(*std::move(clause_or)));
  }
  const mir::ExprId predicate_id =
      BuildMirLogicalAnd(unit, block, bit1_type, clauses);

  auto then_or = BuildConditionalArm(lowerer, frame, c.then_value, result_type);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  auto else_or = BuildConditionalArm(lowerer, frame, c.else_value, result_type);
  if (!else_or) return std::unexpected(std::move(else_or.error()));

  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = ReduceToCondition(unit, block, predicate_id),
              .then_value = *then_or,
              .else_value = *else_or},
      .type = result_type};
}

template <ExprLowerer Lowerer>
auto LowerHirBindingConditionalExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit = lowerer.Owner().Unit();
  const mir::TypeId bit1_type = unit.builtins.bit1;
  auto& block = *frame.current_block;

  // A clause pattern declares an identifier, so this predicate cannot be an
  // rvalue: the arms become assignments into a result local under the same
  // clause chain an `if` uses, and the expression is a read of that local.
  const mir::LocalId result_local =
      frame.bindings->DeclareAnonymous(result_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = result_local,
          .init = block.exprs.Add(
              BuildDefaultValueExpr(unit, block, result_type))});

  // A conditional expression always has both arms, so the else-arm is always
  // reachable and the chain always has to report whether it held.
  const mir::LocalId taken_flag = frame.bindings->DeclareAnonymous(bit1_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = taken_flag, .init = BuildBit1Literal(unit, block, false)});

  auto assign_arm = [&](WalkFrame arm_frame,
                        hir::ExprId value) -> diag::Result<void> {
    auto value_or = BuildConditionalArm(lowerer, arm_frame, value, result_type);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    auto& arm_block = *arm_frame.current_block;
    const mir::ExprId value_id = *value_or;
    const mir::ExprId target =
        arm_block.exprs.Add(mir::MakeLocalRefExpr(result_local, result_type));
    arm_block.AppendStmt(
        mir::ExprStmt{
            .expr = arm_block.exprs.Add(
                mir::MakeAssignExpr(target, value_id, result_type))});
    return {};
  };

  auto chain_or = BuildClauseChainIf(
      lowerer, frame, std::span<const hir::ConditionClause>{c.conditions},
      taken_flag, [&](WalkFrame arm_frame) -> diag::Result<void> {
        return assign_arm(arm_frame, c.then_value);
      });
  if (!chain_or) return std::unexpected(std::move(chain_or.error()));
  block.AppendStmt(*std::move(chain_or));

  mir::Block else_block;
  const WalkFrame else_frame = frame.WithBlock(&else_block);
  auto else_or = assign_arm(else_frame, c.else_value);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::BlockId else_scope = block.child_scopes.Add(std::move(else_block));

  block.AppendStmt(
      BuildChainElseIf(unit, block, taken_flag, bit1_type, else_scope));

  return mir::Expr{
      .data = mir::ReferenceExpr{.target = mir::LocalRef{.var = result_local}},
      .type = result_type};
}

auto LowerHirIncDecExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  // An increment both reads and writes its target, so the target lowers as a
  // write target (LRM 11.4.2), not as a read.
  auto target_or =
      process.LowerLhsExpr(process.HirExprs().Get(inc.target), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  auto& unit = process.Owner().Unit();
  // An increment reads and writes the storage its target reaches, so the target
  // names that storage rather than the wrapper standing for it.
  return mir::Expr{
      .data =
          mir::IncDecExpr{
              .op = LowerIncDecOp(inc.op),
              .target = TargetPlace(unit, block, *target_or)},
      .type = result_type};
}

template <ExprLowerer Lowerer>
auto LowerHirConversionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const hir::Expr& operand = lowerer.HirExprs().Get(cv.operand);
  auto operand_or = lowerer.LowerExpr(operand, frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const auto& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId operand_id = block.exprs.Add(*std::move(operand_or));
  switch (cv.kind) {
    case hir::ConversionKind::kPropagated:
      return BuildPropagatedConversion(unit, block, operand_id, result_type);
    // An assignment extends its right-hand side by that side's own signedness
    // (LRM 11.8.3), and a cast converts the operand to the casting type without
    // restating its signedness first (LRM 6.24.1), so both take the widening
    // every non-propagated context takes.
    case hir::ConversionKind::kImplicit:
    case hir::ConversionKind::kExplicit:
      return BuildValueConversion(unit, block, operand_id, result_type);
    // LRM 6.24.3 reinterprets the operand as a bit stream and repacks it into
    // the casting type, which is a different operation from reshaping one
    // value's representation into another's -- the operand and the result need
    // not even be the same kind of value. The clause states it in two steps and
    // this is those two steps.
    case hir::ConversionKind::kBitstreamCast: {
      auto packed_or = BuildToBitstream(unit, block, operand_id, operand.span);
      if (!packed_or) return std::unexpected(std::move(packed_or.error()));
      return BuildFromBitstream(
          unit, block, *packed_or, result_type, operand.span);
    }
    // LRM 11.4.14 streaming operators, which slang marks as a conversion when
    // one feeds an assignment. The operand is already the stream the operator
    // built, so what is left is the target's own half of the clause: widen to
    // its width, then read the bits back as a value of it.
    case hir::ConversionKind::kStreamingConcat:
      return BuildFromBitstream(
          unit, block, operand_id, result_type, operand.span);
  }
  throw InternalError("LowerHirConversionExpr: unknown hir::ConversionKind");
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
template auto LowerHirBindingConditionalExpr(
    ProcessLowerer&, WalkFrame, const hir::ConditionalExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirBindingConditionalExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ConditionalExpr&,
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
