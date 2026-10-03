#include "lyra/lowering/hir_to_mir/expression/operators.hpp"

#include <array>
#include <expected>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

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
    case hir::BinaryOp::kLogicalOr:
    case hir::BinaryOp::kLogicalImplication:
    case hir::BinaryOp::kLogicalEquivalence:
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
      break;
  }
  throw InternalError(
      "LowerBinaryOp: the operator is not one a target applies to two values "
      "and is settled before a binary node is built");
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

auto MakeLibraryCall(
    support::BuiltinFn entry, mir::ExprId receiver,
    std::vector<mir::ExprId> arguments, mir::TypeId result_type) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry, .receiver = receiver},
              .arguments = std::move(arguments)},
      .type = result_type};
}

// The operator a target applies, or the constant it folds to where every
// operand is one.
auto MakeBinary(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::BinaryOp op,
    mir::ExprId lhs, mir::ExprId rhs, mir::TypeId result_type) -> mir::Expr {
  return FoldedOr(
      unit, mir::FoldBinary(unit, block, op, lhs, rhs, result_type),
      mir::Expr{
          .data = mir::BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs},
          .type = result_type});
}

auto MakeUnary(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::UnaryOp op,
    mir::ExprId operand, mir::TypeId result_type) -> mir::Expr {
  return FoldedOr(
      unit, mir::FoldUnary(unit, block, op, operand, result_type),
      mir::Expr{
          .data = mir::UnaryExpr{.op = op, .operand = operand},
          .type = result_type});
}

template <ExprLowerer Lowerer>
auto LowerAndAddOperand(
    Lowerer& lowerer, const WalkFrame& frame, hir::ExprId id)
    -> diag::Result<mir::ExprId> {
  auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return frame.current_block->exprs.Add(*std::move(lowered));
}

// An operand as what a logical operation of the target or of a library takes:
// such an operation reads an integral operand's truth for itself, whatever its
// width, so only a value of another kind is read for it first.
auto BuildLogicalOperand(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand)
    -> mir::ExprId {
  return unit.types.Get(block.exprs.Get(operand).type).IsIntegralPacked()
             ? operand
             : BuildTruth(unit, block, operand);
}

// `tests` folded by a logical operator that evaluates both of its operands,
// answering with the operator's identity where there is nothing to fold.
auto BuildLogicalFold(
    const mir::CompilationUnit& unit, mir::Block& block, mir::BinaryOp op,
    bool identity, mir::TypeId type, std::span<const mir::ExprId> tests)
    -> mir::ExprId {
  if (tests.empty()) {
    return ConvertToType(
        unit, block, BuildBit1Literal(unit, block, identity), type);
  }
  mir::ExprId folded = tests.front();
  for (const mir::ExprId test : tests.subspan(1)) {
    folded = block.exprs.Add(MakeBinary(unit, block, op, folded, test, type));
  }
  return folded;
}

// The MIR a source-level unary operator names. The operand arrives as an
// expression rather than an id because an operator that names nothing answers
// with it, and interning it first would leave a node nothing reaches.
auto BuildMirUnaryExpr(
    const mir::CompilationUnit& unit, mir::Block& block, hir::UnaryOp op,
    mir::Expr operand, mir::TypeId result_type) -> mir::Expr {
  const auto apply = [&](mir::UnaryOp value_op) {
    return MakeUnary(
        unit, block, value_op, block.exprs.Add(std::move(operand)),
        result_type);
  };
  // LRM 11.4.9: a reduction is an operation over the operand's bits, which a
  // library performs and no target spells as an operator on a value.
  const auto reduce = [&](support::BuiltinFn entry) {
    return MakeLibraryCall(
        entry, block.exprs.Add(std::move(operand)), {}, result_type);
  };
  switch (op) {
    // Unary plus leaves its operand's value unchanged (LRM 11.4.3), so the
    // expression it names is the operand itself.
    case hir::UnaryOp::kPlus:
      return operand;
    case hir::UnaryOp::kMinus:
      return apply(mir::UnaryOp::kMinus);
    case hir::UnaryOp::kBitwiseNot:
      return apply(mir::UnaryOp::kBitwiseNot);
    // LRM 11.4.7 negates the operand's truth, and a real (LRM 11.3.1) or a
    // handle (LRM 6.14, 8.4) has one as well as an integral value does.
    case hir::UnaryOp::kLogicalNot:
      return MakeUnary(
          unit, block, mir::UnaryOp::kLogicalNot,
          BuildLogicalOperand(unit, block, block.exprs.Add(std::move(operand))),
          result_type);
    case hir::UnaryOp::kReductionAnd:
      return reduce(support::BuiltinFn::kReductionAnd);
    case hir::UnaryOp::kReductionOr:
      return reduce(support::BuiltinFn::kReductionOr);
    case hir::UnaryOp::kReductionXor:
      return reduce(support::BuiltinFn::kReductionXor);
    case hir::UnaryOp::kReductionNand:
      return reduce(support::BuiltinFn::kReductionNand);
    case hir::UnaryOp::kReductionNor:
      return reduce(support::BuiltinFn::kReductionNor);
    case hir::UnaryOp::kReductionXnor:
      return reduce(support::BuiltinFn::kReductionXnor);
  }
  throw InternalError("BuildMirUnaryExpr: unknown HIR UnaryOp");
}

}  // namespace

auto BuildMirLogicalAnd(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    std::span<const mir::ExprId> tests) -> mir::ExprId {
  return BuildLogicalFold(
      unit, block, mir::BinaryOp::kLogicalAnd, true, type, tests);
}

auto BuildMirLogicalOr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    std::span<const mir::ExprId> tests) -> mir::ExprId {
  return BuildLogicalFold(
      unit, block, mir::BinaryOp::kLogicalOr, false, type, tests);
}

auto BuildMirBinaryExpr(
    const mir::CompilationUnit& unit, mir::Block& block, hir::BinaryOp op,
    mir::ExprId lhs_id, mir::ExprId rhs_id, mir::TypeId result_type)
    -> mir::Expr {
  const mir::TypeId lhs_type = block.exprs.Get(lhs_id).type;
  const mir::TypeId rhs_type = block.exprs.Get(rhs_id).type;

  const auto at_result_type = [&](mir::ExprId answer) {
    return block.exprs.Get(ConvertToType(unit, block, answer, result_type));
  };
  const auto apply = [&](mir::BinaryOp value_op) {
    return MakeBinary(unit, block, value_op, lhs_id, rhs_id, result_type);
  };
  // The entry that performs an operator a target cannot apply to two values of
  // one type. Three reasons put one here: its operands are not two values of
  // one type (a shift's amount is sized on its own, LRM 11.4.10); it reads a
  // value's representation rather than its value (a wildcard equality compares
  // x and z as themselves, LRM 11.4.6); or it composes several operations into
  // one (power, xnor).
  const auto library = [&](support::BuiltinFn entry) {
    return MakeLibraryCall(entry, lhs_id, {rhs_id}, result_type);
  };
  const auto negated = [&](mir::ExprId answer) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::UnaryExpr{
                    .op = mir::UnaryOp::kLogicalNot, .operand = answer},
            .type = block.exprs.Get(answer).type});
  };

  // LRM 8.4 admits `null` as either operand of a handle comparison, and the
  // front end leaves it at a type of its own, so both operands of an equality
  // are brought to the handle's type where one of them is a handle. Operands of
  // any other kind are left as they are.
  struct Comparands {
    mir::ExprId lhs;
    mir::ExprId rhs;
  };
  const auto comparands = [&] {
    const mir::TypeId compared_at =
        unit.types.Get(lhs_type).Is<mir::ManagedRefType>() ? lhs_type
                                                           : rhs_type;
    return Comparands{
        .lhs = OperandAtHandleType(unit, block, lhs_id, compared_at),
        .rhs = OperandAtHandleType(unit, block, rhs_id, compared_at)};
  };
  // LRM 11.4.5 `==` and `!=`. A struct's are its type's own answers, stated at
  // the width and state each has.
  const auto logical_equality = [&](mir::BinaryOp value_op,
                                    support::ValueOperator comparison) {
    const Comparands compared = comparands();
    if (unit.types.Get(lhs_type).Is<mir::StructType>()) {
      return at_result_type(BuildStructComparison(
          unit, block, comparison, compared.lhs, compared.rhs));
    }
    return MakeBinary(
        unit, block, value_op, compared.lhs, compared.rhs, result_type);
  };
  // LRM 11.4.5 `===` over any data type, an aggregate included. The clause
  // makes the result always a known 1'b0 or 1'b1, so the question answers with
  // a two-state bit whatever its operands carry, and a context wanting another
  // representation takes the conversion.
  const auto case_equality = [&] {
    const Comparands compared = comparands();
    return BuildCaseEquality(unit, block, compared.lhs, compared.rhs);
  };

  switch (op) {
    case hir::BinaryOp::kAdd:
    case hir::BinaryOp::kSub:
    case hir::BinaryOp::kMul:
    case hir::BinaryOp::kDiv:
    case hir::BinaryOp::kMod:
    case hir::BinaryOp::kBitwiseAnd:
    case hir::BinaryOp::kBitwiseOr:
    case hir::BinaryOp::kBitwiseXor:
    case hir::BinaryOp::kGreaterEqual:
    case hir::BinaryOp::kGreaterThan:
    case hir::BinaryOp::kLessEqual:
    case hir::BinaryOp::kLessThan:
      return apply(LowerBinaryOp(op));
    case hir::BinaryOp::kEquality:
      return logical_equality(
          mir::BinaryOp::kEquality, support::ValueOperator::kEquality);
    case hir::BinaryOp::kInequality:
      return logical_equality(
          mir::BinaryOp::kInequality, support::ValueOperator::kInequality);
    case hir::BinaryOp::kCaseEquality:
      return at_result_type(case_equality());
    case hir::BinaryOp::kCaseInequality:
      return at_result_type(negated(case_equality()));
    case hir::BinaryOp::kWildcardEquality:
      return library(support::BuiltinFn::kWildcardEquals);
    // SV spells `!=?` as an operator of its own, answering with the negation of
    // the comparison beside it (LRM 11.4.6). Unlike case inequality it takes no
    // conversion: the clause lets the answer be unknown, so it carries the
    // state class its operands do, which is the one the context asked for.
    case hir::BinaryOp::kWildcardInequality:
      return block.exprs.Get(negated(
          block.exprs.Add(library(support::BuiltinFn::kWildcardEquals))));
    case hir::BinaryOp::kPower:
      return library(support::BuiltinFn::kPow);
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return library(support::BuiltinFn::kShiftLeft);
    case hir::BinaryOp::kLogicalShiftRight:
      return library(support::BuiltinFn::kLogicalShiftRight);
    case hir::BinaryOp::kArithmeticShiftRight:
      return library(support::BuiltinFn::kArithmeticShiftRight);
    case hir::BinaryOp::kBitwiseXnor:
      return library(support::BuiltinFn::kBitwiseXnor);
    // LRM 11.4.7 evaluates both operands of `<->` exactly once and compares
    // their truths, and the answer can be unknown exactly where an operand can.
    case hir::BinaryOp::kLogicalEquivalence:
      return at_result_type(block.exprs.Add(MakeLibraryCall(
          support::BuiltinFn::kLogicalEquivalence,
          BuildLogicalOperand(unit, block, lhs_id),
          {BuildLogicalOperand(unit, block, rhs_id)},
          OneBitAnswerType(unit, std::array{lhs_type, rhs_type}))));
    case hir::BinaryOp::kLogicalAnd:
    case hir::BinaryOp::kLogicalOr:
    case hir::BinaryOp::kLogicalImplication:
      throw InternalError(
          "BuildMirBinaryExpr: the operator may leave its second operand "
          "unevaluated, so it is a selection built before both are lowered");
  }
  throw InternalError("BuildMirBinaryExpr: unknown HIR BinaryOp");
}

template <ExprLowerer Lowerer>
auto LowerHirUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto operand_or = lowerer.LowerExpr(lowerer.HirExprs().Get(u.operand), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  return BuildMirUnaryExpr(
      lowerer.Owner().Unit(), *frame.current_block, u.op,
      *std::move(operand_or), result_type);
}

// `&&`, `||` and `->` evaluate their second operand only where the first leaves
// the answer open, and an unknown first operand leaves it open: the second is
// evaluated and the two combine by the operator's table (LRM 11.4.7, 11.3.5).
// That is the conditional operator over the first operand (LRM 11.4.11), with
// the second operand's truth as one arm and the answer the first settles as
// the other, since combining the two arms bit by bit is that table. Every other
// operator evaluates both operands.
template <ExprLowerer Lowerer>
auto LowerHirBinaryExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const Evaluation second_truth =
      [&](const WalkFrame& at) -> diag::Result<mir::ExprId> {
    auto second_or = LowerAndAddOperand(lowerer, at, b.rhs);
    if (!second_or) return second_or;
    mir::Block& block = *at.current_block;
    return ConvertToType(
        unit, block, BuildTruth(unit, block, *second_or), result_type);
  };
  const auto settled = [&unit, result_type](bool answer) -> Evaluation {
    return [&unit, result_type,
            answer](const WalkFrame& at) -> diag::Result<mir::ExprId> {
      mir::Block& block = *at.current_block;
      return ConvertToType(
          unit, block, BuildBit1Literal(unit, block, answer), result_type);
    };
  };
  const auto select_on_first = [&](const Evaluation& then_arm,
                                   const Evaluation& else_arm) {
    return BuildSelection(
        unit, frame, ExpressionPredicate(lowerer, b.lhs), result_type, then_arm,
        else_arm);
  };
  switch (b.op) {
    case hir::BinaryOp::kLogicalAnd:
      return select_on_first(second_truth, settled(false));
    case hir::BinaryOp::kLogicalOr:
      return select_on_first(settled(true), second_truth);
    case hir::BinaryOp::kLogicalImplication:
      return select_on_first(second_truth, settled(true));
    case hir::BinaryOp::kAdd:
    case hir::BinaryOp::kSub:
    case hir::BinaryOp::kMul:
    case hir::BinaryOp::kDiv:
    case hir::BinaryOp::kMod:
    case hir::BinaryOp::kBitwiseAnd:
    case hir::BinaryOp::kBitwiseOr:
    case hir::BinaryOp::kBitwiseXor:
    case hir::BinaryOp::kBitwiseXnor:
    case hir::BinaryOp::kEquality:
    case hir::BinaryOp::kInequality:
    case hir::BinaryOp::kCaseEquality:
    case hir::BinaryOp::kCaseInequality:
    case hir::BinaryOp::kWildcardEquality:
    case hir::BinaryOp::kWildcardInequality:
    case hir::BinaryOp::kGreaterEqual:
    case hir::BinaryOp::kGreaterThan:
    case hir::BinaryOp::kLessEqual:
    case hir::BinaryOp::kLessThan:
    case hir::BinaryOp::kLogicalEquivalence:
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
    case hir::BinaryOp::kLogicalShiftRight:
    case hir::BinaryOp::kArithmeticShiftRight:
    case hir::BinaryOp::kPower:
      break;
  }
  auto lhs_or = LowerAndAddOperand(lowerer, frame, b.lhs);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  auto rhs_or = LowerAndAddOperand(lowerer, frame, b.rhs);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  return BuildMirBinaryExpr(
      unit, *frame.current_block, b.op, *lhs_or, *rhs_or, result_type);
}

// The conditional operator: a selection on its predicate, a series of clauses,
// between its two expressions (LRM 11.4.11, 12.6.3). Both arms carry the type
// the conditional yields whatever their own expressions produced, because what
// reads a conditional reads the type off the node. The identifiers a clause's
// pattern introduces are declared in the block the expression stands in, which
// encloses the clauses after it and the first expression, the two things that
// may read them.
template <ExprLowerer Lowerer>
auto LowerHirConditionalExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto arm = [&lowerer, &unit,
                    result_type](hir::ExprId value) -> Evaluation {
    return [&lowerer, &unit, value,
            result_type](const WalkFrame& at) -> diag::Result<mir::ExprId> {
      auto value_or = LowerAndAddOperand(lowerer, at, value);
      if (!value_or) return value_or;
      return ConvertToType(unit, *at.current_block, *value_or, result_type);
    };
  };
  return BuildSelection(
      unit, frame, ClauseSeriesPredicate(lowerer, frame, c.conditions),
      result_type, arm(c.then_value), arm(c.else_value));
}

template <ExprLowerer Lowerer>
auto LowerHirIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  // An increment both reads and writes its target (LRM 11.4.2), so the target
  // lowers as a write target and names the storage it reaches rather than the
  // wrapper standing for it.
  auto target_or =
      lowerer.LowerLhsExpr(lowerer.HirExprs().Get(inc.target), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  return mir::Expr{
      .data =
          mir::IncDecExpr{
              .op = LowerIncDecOp(inc.op),
              .target = PathPlace(
                  lowerer.Owner().Unit(), *frame.current_block, *target_or)},
      .type = result_type};
}

template <ExprLowerer Lowerer>
auto LowerHirConversionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const diag::SourceSpan span = lowerer.HirExprs().Get(cv.operand).span;
  auto operand_or = LowerAndAddOperand(lowerer, frame, cv.operand);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const mir::ExprId operand_id = *operand_or;
  const auto& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
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
      auto packed_or = BuildToBitstream(unit, block, operand_id, span);
      if (!packed_or) return std::unexpected(std::move(packed_or.error()));
      auto value_or =
          BuildFromBitstream(unit, block, *packed_or, result_type, span);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      return block.exprs.Get(*value_or);
    }
    // LRM 11.4.14 streaming operators, which slang marks as a conversion when
    // one feeds an assignment. The operand is already the stream the operator
    // built, so what is left is the target's own half of the clause: widen to
    // its width, then read the bits back as a value of it.
    case hir::ConversionKind::kStreamingConcat: {
      auto value_or =
          BuildFromBitstream(unit, block, operand_id, result_type, span);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      return block.exprs.Get(*value_or);
    }
  }
  throw InternalError("LowerHirConversionExpr: unknown hir::ConversionKind");
}

// One instantiation per pass class. The templates are defined in this file
// rather than the header so the file-local helpers stay private.
template auto LowerHirUnaryExpr(
    ProcessLowerer&, WalkFrame, const hir::UnaryExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirUnaryExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::UnaryExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirIncDecExpr(
    ProcessLowerer&, WalkFrame, const hir::IncDecExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirIncDecExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::IncDecExpr&,
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
