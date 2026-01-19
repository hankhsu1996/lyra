#include "lyra/lowering/hir_to_mir/expression.hpp"

#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowering Policy: Temp Materialization
//
// Leaf expressions (constants, name refs) produce Operands directly.
// Non-leaf expressions (unary, binary, calls) always materialize their
// result into a temp via EmitTemp, then return Use(temp).
//
// This avoids value identity issues (MIR has no SSA) and keeps the lowering
// uniform. Future expression kinds (casts, selects, bit-slices) should
// follow the same pattern.

namespace {

auto LowerConstant(const hir::ConstantExpressionData& data, MirBuilder& builder)
    -> mir::Operand {
  const Constant& constant =
      (*builder.GetContext().constant_arena)[data.constant];
  return mir::Operand::Const(constant);
}

auto LowerNameRef(const hir::NameRefExpressionData& data, MirBuilder& builder)
    -> mir::Operand {
  mir::PlaceId place_id = builder.GetContext().LookupPlace(data.symbol);
  return mir::Operand::Use(place_id);
}

auto MapUnaryOp(hir::UnaryOp op) -> mir::UnaryOp {
  switch (op) {
    case hir::UnaryOp::kPlus:
      return mir::UnaryOp::kPlus;
    case hir::UnaryOp::kMinus:
      return mir::UnaryOp::kMinus;
    case hir::UnaryOp::kPreincrement:
      return mir::UnaryOp::kPreincrement;
    case hir::UnaryOp::kPostincrement:
      return mir::UnaryOp::kPostincrement;
    case hir::UnaryOp::kPredecrement:
      return mir::UnaryOp::kPredecrement;
    case hir::UnaryOp::kPostdecrement:
      return mir::UnaryOp::kPostdecrement;
    case hir::UnaryOp::kLogicalNot:
      return mir::UnaryOp::kLogicalNot;
    case hir::UnaryOp::kBitwiseNot:
      return mir::UnaryOp::kBitwiseNot;
    case hir::UnaryOp::kReductionAnd:
      return mir::UnaryOp::kReductionAnd;
    case hir::UnaryOp::kReductionNand:
      return mir::UnaryOp::kReductionNand;
    case hir::UnaryOp::kReductionOr:
      return mir::UnaryOp::kReductionOr;
    case hir::UnaryOp::kReductionNor:
      return mir::UnaryOp::kReductionNor;
    case hir::UnaryOp::kReductionXor:
      return mir::UnaryOp::kReductionXor;
    case hir::UnaryOp::kReductionXnor:
      return mir::UnaryOp::kReductionXnor;
  }
  throw common::InternalError("MapUnaryOp", "unknown unary op");
}

auto MapBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
    case hir::BinaryOp::kSubtract:
      return mir::BinaryOp::kSubtract;
    case hir::BinaryOp::kMultiply:
      return mir::BinaryOp::kMultiply;
    case hir::BinaryOp::kDivide:
      return mir::BinaryOp::kDivide;
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
    case hir::BinaryOp::kLogicalAnd:
      return mir::BinaryOp::kLogicalAnd;
    case hir::BinaryOp::kLogicalOr:
      return mir::BinaryOp::kLogicalOr;
    case hir::BinaryOp::kLogicalImplication:
      return mir::BinaryOp::kLogicalImplication;
    case hir::BinaryOp::kLogicalEquivalence:
      return mir::BinaryOp::kLogicalEquivalence;
    case hir::BinaryOp::kEqual:
      return mir::BinaryOp::kEqual;
    case hir::BinaryOp::kNotEqual:
      return mir::BinaryOp::kNotEqual;
    case hir::BinaryOp::kCaseEqual:
      return mir::BinaryOp::kCaseEqual;
    case hir::BinaryOp::kCaseNotEqual:
      return mir::BinaryOp::kCaseNotEqual;
    case hir::BinaryOp::kWildcardEqual:
      return mir::BinaryOp::kWildcardEqual;
    case hir::BinaryOp::kWildcardNotEqual:
      return mir::BinaryOp::kWildcardNotEqual;
    case hir::BinaryOp::kLessThan:
      return mir::BinaryOp::kLessThan;
    case hir::BinaryOp::kLessThanEqual:
      return mir::BinaryOp::kLessThanEqual;
    case hir::BinaryOp::kGreaterThan:
      return mir::BinaryOp::kGreaterThan;
    case hir::BinaryOp::kGreaterThanEqual:
      return mir::BinaryOp::kGreaterThanEqual;
    case hir::BinaryOp::kLogicalShiftLeft:
      return mir::BinaryOp::kLogicalShiftLeft;
    case hir::BinaryOp::kLogicalShiftRight:
      return mir::BinaryOp::kLogicalShiftRight;
    case hir::BinaryOp::kArithmeticShiftLeft:
      return mir::BinaryOp::kArithmeticShiftLeft;
    case hir::BinaryOp::kArithmeticShiftRight:
      return mir::BinaryOp::kArithmeticShiftRight;
  }
  throw common::InternalError("MapBinaryOp", "unknown binary op");
}

auto IsRelationalOp(hir::BinaryOp op) -> bool {
  using BO = hir::BinaryOp;
  return op == BO::kLessThan || op == BO::kLessThanEqual ||
         op == BO::kGreaterThan || op == BO::kGreaterThanEqual;
}

auto IsDivisionOrModuloOp(hir::BinaryOp op) -> bool {
  using BO = hir::BinaryOp;
  return op == BO::kDivide || op == BO::kMod;
}

auto ToSignedVariant(mir::BinaryOp op) -> mir::BinaryOp {
  using BO = mir::BinaryOp;
  switch (op) {
    case BO::kLessThan:
      return BO::kLessThanSigned;
    case BO::kLessThanEqual:
      return BO::kLessThanEqualSigned;
    case BO::kGreaterThan:
      return BO::kGreaterThanSigned;
    case BO::kGreaterThanEqual:
      return BO::kGreaterThanEqualSigned;
    case BO::kDivide:
      return BO::kDivideSigned;
    case BO::kMod:
      return BO::kModSigned;
    default:
      return op;
  }
}

// Select the MIR comparison operator for relational ops (<, <=, >, >=).
// Signed/unsigned variants only apply to integral types. Non-integral types
// (strings, enums, etc.) use the base operator - the interpreter dispatches
// on runtime type. Type checking is based on resolved Lyra TypeId, so
// typedefs are handled correctly.
auto SelectComparisonOp(
    const hir::BinaryExpressionData& data, const Context& ctx)
    -> mir::BinaryOp {
  const auto& lhs_type = (*ctx.type_arena)[(*ctx.hir_arena)[data.lhs].type];
  const auto& rhs_type = (*ctx.type_arena)[(*ctx.hir_arena)[data.rhs].type];

  auto mir_op = MapBinaryOp(data.op);

  // Only integral types have signed/unsigned variants
  if (lhs_type.Kind() != TypeKind::kIntegral ||
      rhs_type.Kind() != TypeKind::kIntegral) {
    return mir_op;
  }

  const auto& lhs = lhs_type.AsIntegral();
  const auto& rhs = rhs_type.AsIntegral();
  if (lhs.is_signed != rhs.is_signed) {
    throw common::InternalError(
        "SelectComparisonOp",
        "operand signedness mismatch - missing conversion");
  }

  return lhs.is_signed ? ToSignedVariant(mir_op) : mir_op;
}

// Select the MIR operator for division/modulo ops.
// Signed/unsigned variants only apply to integral types.
auto SelectDivModOp(const hir::BinaryExpressionData& data, const Context& ctx)
    -> mir::BinaryOp {
  const auto& lhs_type = (*ctx.type_arena)[(*ctx.hir_arena)[data.lhs].type];
  const auto& rhs_type = (*ctx.type_arena)[(*ctx.hir_arena)[data.rhs].type];

  auto mir_op = MapBinaryOp(data.op);

  // Only integral types have signed/unsigned variants
  if (lhs_type.Kind() != TypeKind::kIntegral ||
      rhs_type.Kind() != TypeKind::kIntegral) {
    return mir_op;
  }

  const auto& lhs = lhs_type.AsIntegral();
  const auto& rhs = rhs_type.AsIntegral();
  if (lhs.is_signed != rhs.is_signed) {
    throw common::InternalError(
        "SelectDivModOp", "operand signedness mismatch - missing conversion");
  }

  return lhs.is_signed ? ToSignedVariant(mir_op) : mir_op;
}

auto IsIncrementOrDecrement(hir::UnaryOp op) -> bool {
  using UO = hir::UnaryOp;
  return op == UO::kPreincrement || op == UO::kPostincrement ||
         op == UO::kPredecrement || op == UO::kPostdecrement;
}

// Creates typed constant 1 for increment/decrement desugaring.
// The constant carries the type so the interpreter can determine bit width.
auto MakeOne(TypeId type) -> Constant {
  IntegralConstant ic;
  ic.value.push_back(1);
  return Constant{.type = type, .value = std::move(ic)};
}

// Forward declaration for OOB default helper used by increment/decrement.
auto MakeOobDefault(TypeId element_type, uint32_t width, const TypeArena& types)
    -> mir::Operand;

// Desugar increment/decrement operators into explicit read-modify-write.
// These operators have side effects (modify the operand) and return a value
// (pre returns new value, post returns old value). MIR's Place/Operand
// separation doesn't allow side-effecting Rvalues, so we desugar here.
auto LowerIncrementDecrement(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // 1. Lower operand as lvalue
  LvalueResult target = LowerLvalue(data.operand, builder);

  // 2. Type validation (defensive - Slang already checked)
  const Type& type = (*ctx.type_arena)[expr.type];
  if (type.Kind() != TypeKind::kIntegral) {
    throw common::InternalError(
        "LowerIncrementDecrement", "operand must be integral type");
  }

  // 3. Read old value with OOB handling
  mir::Operand read_val;
  if (IsAlwaysValid(target.validity)) {
    read_val = mir::Operand::Use(target.place);
  } else {
    // OOB/X/Z index: use Select to return X (4-state) or 0 (2-state)
    mir::Operand oob_default =
        MakeOobDefault(expr.type, type.AsIntegral().bit_width, *ctx.type_arena);
    read_val = builder.EmitSelect(
        target.validity, mir::Operand::Use(target.place), oob_default,
        expr.type);
  }
  mir::PlaceId old_value = ctx.AllocTemp(expr.type);
  builder.EmitAssign(old_value, read_val);

  // 4. Compute new value using existing binary op machinery
  bool is_increment =
      (data.op == hir::UnaryOp::kPreincrement ||
       data.op == hir::UnaryOp::kPostincrement);
  mir::BinaryOp bin_op =
      is_increment ? mir::BinaryOp::kAdd : mir::BinaryOp::kSubtract;

  // Create typed constant 1 - type is needed for interpreter to determine width
  Constant one = MakeOne(expr.type);

  mir::Rvalue compute_rvalue{
      .operands = {mir::Operand::Use(old_value), mir::Operand::Const(one)},
      .info = mir::BinaryRvalueInfo{.op = bin_op},
  };
  mir::PlaceId new_value =
      builder.EmitTemp(expr.type, std::move(compute_rvalue));

  // 5. Write back to target with guarded store for OOB safety
  if (IsAlwaysValid(target.validity)) {
    builder.EmitAssign(target.place, mir::Operand::Use(new_value));
  } else {
    mir::Operand cond = target.validity;

    // EmitBranch requires Use operand - materialize constant if needed
    if (cond.kind == mir::Operand::Kind::kConst) {
      mir::PlaceId temp = ctx.AllocTemp(ctx.GetBitType());
      builder.EmitAssign(temp, std::move(cond));
      cond = mir::Operand::Use(temp);
    }

    BlockIndex store_bb = builder.CreateBlock();
    BlockIndex merge_bb = builder.CreateBlock();
    builder.EmitBranch(cond, store_bb, merge_bb);

    builder.SetCurrentBlock(store_bb);
    builder.EmitAssign(target.place, mir::Operand::Use(new_value));
    builder.EmitJump(merge_bb);

    builder.SetCurrentBlock(merge_bb);
  }

  // 6. Return appropriate value
  bool is_pre =
      (data.op == hir::UnaryOp::kPreincrement ||
       data.op == hir::UnaryOp::kPredecrement);
  return mir::Operand::Use(is_pre ? new_value : old_value);
}

auto LowerUnary(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  // Desugar increment/decrement operators into read-modify-write sequence
  if (IsIncrementOrDecrement(data.op)) {
    return LowerIncrementDecrement(data, expr, builder);
  }

  mir::Operand operand = LowerExpression(data.operand, builder);

  mir::Rvalue rvalue{
      .operands = {operand},
      .info = mir::UnaryRvalueInfo{.op = MapUnaryOp(data.op)},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerBinary(
    const hir::BinaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  mir::Operand lhs = LowerExpression(data.lhs, builder);
  mir::Operand rhs = LowerExpression(data.rhs, builder);

  auto select_mir_op = [&]() -> mir::BinaryOp {
    if (IsRelationalOp(data.op)) {
      return SelectComparisonOp(data, builder.GetContext());
    }
    if (IsDivisionOrModuloOp(data.op)) {
      return SelectDivModOp(data, builder.GetContext());
    }
    return MapBinaryOp(data.op);
  };
  mir::BinaryOp mir_op = select_mir_op();

  mir::Rvalue rvalue{
      .operands = {lhs, rhs},
      .info = mir::BinaryRvalueInfo{.op = mir_op},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerCast(
    const hir::CastExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  // Invariant: caller dispatched based on CastExpressionData
  if (expr.kind != hir::ExpressionKind::kCast) {
    throw common::InternalError("LowerCast", "expected kCast expression kind");
  }

  // Lower operand first. Note: current lowering is structure-preserving (no
  // rewriting), so we can safely read source type from HIR after lowering.
  mir::Operand operand = LowerExpression(data.operand, builder);

  const Context& ctx = builder.GetContext();
  const hir::Expression& operand_expr = (*ctx.hir_arena)[data.operand];
  TypeId source_type = operand_expr.type;
  TypeId target_type = expr.type;

  const auto& src = (*ctx.type_arena)[source_type];
  const auto& tgt = (*ctx.type_arena)[target_type];

  // These checks are invariants - unsupported types should have been rejected
  // in AST->HIR lowering. If we reach here, it's a compiler bug.
  // Both kIntegral and kPackedArray are valid for casts (both are packed).
  if (!IsPacked(src)) {
    throw common::InternalError(
        "LowerCast", "non-packed source should have been rejected in AST->HIR");
  }
  if (!IsPacked(tgt)) {
    throw common::InternalError(
        "LowerCast", "non-packed target should have been rejected in AST->HIR");
  }

  if (IsPackedFourState(src, *ctx.type_arena)) {
    throw common::InternalError(
        "LowerCast", "4-state source should have been rejected in AST->HIR");
  }
  if (IsPackedFourState(tgt, *ctx.type_arena)) {
    throw common::InternalError(
        "LowerCast", "4-state target should have been rejected in AST->HIR");
  }

  mir::Rvalue rvalue{
      .operands = {operand},
      .info =
          mir::CastRvalueInfo{
              .source_type = source_type, .target_type = target_type},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerSystemCall(
    const hir::SystemCallExpressionData& /*data*/,
    const hir::Expression& /*expr*/, MirBuilder& /*builder*/) -> mir::Operand {
  // Effect system calls ($display, etc.) are handled in statement.cpp
  // as Effect instructions. If we get here, it's because a system call
  // was used in an expression context where a value is expected.
  // Currently all supported system calls are effects, so this is an error.
  throw common::InternalError(
      "LowerSystemCall",
      "system call used in value context (only effect calls supported)");
}

auto LowerAssignment(
    const hir::AssignmentExpressionData& data, const hir::Expression& /*expr*/,
    MirBuilder& builder) -> mir::Operand {
  // Lower target as lvalue
  LvalueResult target = LowerLvalue(data.target, builder);

  // Lower value as rvalue
  mir::Operand value = LowerExpression(data.value, builder);

  if (IsAlwaysValid(target.validity)) {
    builder.EmitAssign(target.place, value);
    return value;
  }

  // Guarded store: only write if validity is true (OOB/X/Z = no-op)
  Context& ctx = builder.GetContext();
  mir::Operand cond = target.validity;

  // EmitBranch requires Use operand - materialize constant if needed
  if (cond.kind == mir::Operand::Kind::kConst) {
    mir::PlaceId temp = ctx.AllocTemp(ctx.GetBitType());
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

  BlockIndex store_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();
  builder.EmitBranch(cond, store_bb, merge_bb);

  builder.SetCurrentBlock(store_bb);
  builder.EmitAssign(target.place, value);
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);

  // Return the value (assignment expression yields the assigned value)
  return value;
}

// Lower ternary operator (a ? b : c) to control flow.
// MIR has no "select" instruction, so we lower to branches:
//   result = _
//   branch cond -> then_bb, else_bb
//   then_bb: result = then_val; jump merge_bb
//   else_bb: result = else_val; jump merge_bb
//   merge_bb: (continue here)
//
// Short-circuit: arms are lowered INSIDE their branch blocks, ensuring
// only the taken arm is evaluated.
auto LowerConditional(
    const hir::ConditionalExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // 1. Allocate result temp BEFORE branching (dominates both arms and merge)
  mir::PlaceId result = ctx.AllocTemp(expr.type);

  // 2. Lower condition (must not emit terminator - currently safe since
  //    logical ops use binary instructions, not control flow)
  mir::Operand cond = LowerExpression(data.condition, builder);

  // 3. EmitBranch requires Use operand; materialize if needed
  //    Use condition's type (not expr.type) for the temp
  if (cond.kind != mir::Operand::Kind::kUse) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

  // 4. Create blocks and emit branch (terminates current block)
  BlockIndex then_bb = builder.CreateBlock();
  BlockIndex else_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();
  builder.EmitBranch(cond, then_bb, else_bb);

  // 5. Then block: lower then_expr HERE (short-circuit), assign, terminate
  builder.SetCurrentBlock(then_bb);
  mir::Operand then_val = LowerExpression(data.then_expr, builder);
  builder.EmitAssign(result, std::move(then_val));
  builder.EmitJump(merge_bb);

  // 6. Else block: lower else_expr HERE (short-circuit), assign, terminate
  builder.SetCurrentBlock(else_bb);
  mir::Operand else_val = LowerExpression(data.else_expr, builder);
  builder.EmitAssign(result, std::move(else_val));
  builder.EmitJump(merge_bb);

  // 7. Continue in merge block
  builder.SetCurrentBlock(merge_bb);
  return mir::Operand::Use(result);
}

auto LowerStructLiteral(
    const hir::StructLiteralExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  std::vector<mir::Operand> operands;
  operands.reserve(data.field_values.size());
  for (hir::ExpressionId field_id : data.field_values) {
    operands.push_back(LowerExpression(field_id, builder));
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::AggregateRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
}

auto LowerCall(
    const hir::CallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // Look up mir::FunctionId from symbol
  mir::FunctionId callee = ctx.LookupFunction(data.callee);
  if (!callee) {
    throw common::InternalError("LowerCall", "function not found in map");
  }

  // Lower arguments
  std::vector<mir::Operand> args;
  args.reserve(data.arguments.size());
  for (hir::ExpressionId arg_id : data.arguments) {
    args.push_back(LowerExpression(arg_id, builder));
  }

  mir::Rvalue rvalue{
      .operands = std::move(args),
      .info = mir::UserCallRvalueInfo{.callee = callee},
  };

  mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp);
}

auto LowerNewArray(
    const hir::NewArrayExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  std::vector<mir::Operand> operands;
  operands.push_back(LowerExpression(data.size_expr, builder));
  if (data.init_expr) {
    operands.push_back(LowerExpression(*data.init_expr, builder));
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info =
          mir::BuiltinCallRvalueInfo{
              .method = mir::BuiltinMethod::kNewArray,
              .result_type = expr.type,
              .receiver = std::nullopt},
  };

  mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp);
}

auto LowerArrayLiteral(
    const hir::ArrayLiteralExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  std::vector<mir::Operand> operands;
  operands.reserve(data.elements.size());
  for (hir::ExpressionId elem_id : data.elements) {
    operands.push_back(LowerExpression(elem_id, builder));
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::AggregateRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
}

auto LowerBuiltinMethodCall(
    const hir::BuiltinMethodCallExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();
  const hir::Expression& receiver_expr = (*ctx.hir_arena)[data.receiver];

  switch (data.method) {
    case hir::BuiltinMethod::kSize: {
      mir::Operand receiver = LowerExpression(data.receiver, builder);
      // Determine which size method based on receiver type
      const Type& receiver_type = (*ctx.type_arena)[receiver_expr.type];
      mir::BuiltinMethod method = (receiver_type.Kind() == TypeKind::kQueue)
                                      ? mir::BuiltinMethod::kQueueSize
                                      : mir::BuiltinMethod::kArraySize;
      mir::Rvalue rvalue{
          .operands = {receiver},
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = method,
                  .result_type = expr.type,
                  .receiver = std::nullopt},
      };
      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kPopBack:
    case hir::BuiltinMethod::kPopFront: {
      // Pop methods mutate the receiver AND return a value
      // Need receiver as PlaceId for mutation
      LvalueResult lv = LowerLvalue(data.receiver, builder);

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kPopBack)
                                      ? mir::BuiltinMethod::kQueuePopBack
                                      : mir::BuiltinMethod::kQueuePopFront;

      mir::Rvalue rvalue{
          .operands = {},
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = method,
                  .result_type = expr.type,
                  .receiver = lv.place},
      };
      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kPushBack:
    case hir::BuiltinMethod::kPushFront: {
      LvalueResult lv = LowerLvalue(data.receiver, builder);
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        operands.push_back(LowerExpression(arg_id, builder));
      }

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kPushBack)
                                      ? mir::BuiltinMethod::kQueuePushBack
                                      : mir::BuiltinMethod::kQueuePushFront;

      mir::Rvalue rvalue{
          .operands = std::move(operands),
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = method,
                  .result_type = expr.type,
                  .receiver = lv.place},
      };

      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kInsert: {
      LvalueResult lv = LowerLvalue(data.receiver, builder);
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        operands.push_back(LowerExpression(arg_id, builder));
      }

      mir::Rvalue rvalue{
          .operands = std::move(operands),
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = mir::BuiltinMethod::kQueueInsert,
                  .result_type = expr.type,
                  .receiver = lv.place},
      };

      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kDelete: {
      LvalueResult lv = LowerLvalue(data.receiver, builder);
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        operands.push_back(LowerExpression(arg_id, builder));
      }

      // Determine method: kArrayDelete, kQueueDelete, or kQueueDeleteAt
      const Type& receiver_type = (*ctx.type_arena)[receiver_expr.type];
      mir::BuiltinMethod method = mir::BuiltinMethod::kArrayDelete;
      if (receiver_type.Kind() == TypeKind::kDynamicArray) {
        method = mir::BuiltinMethod::kArrayDelete;
      } else if (operands.empty()) {
        method = mir::BuiltinMethod::kQueueDelete;
      } else {
        method = mir::BuiltinMethod::kQueueDeleteAt;
      }

      mir::Rvalue rvalue{
          .operands = std::move(operands),
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = method,
                  .result_type = expr.type,
                  .receiver = lv.place},
      };

      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }
  }
  throw common::InternalError(
      "LowerBuiltinMethodCall", "unknown builtin method");
}

// Get the base place from an operand, materializing to temp if needed.
auto GetOrMaterializePlace(
    mir::Operand& operand, TypeId type, MirBuilder& builder) -> mir::PlaceId {
  if (operand.kind == mir::Operand::Kind::kUse) {
    return std::get<mir::PlaceId>(operand.payload);
  }
  if (operand.kind == mir::Operand::Kind::kConst) {
    return builder.EmitTempAssign(type, operand);
  }
  throw common::InternalError(
      "GetOrMaterializePlace", "unexpected operand kind");
}

// Get a 1-bit 2-state type for boolean results.
auto GetBoolType(const Context& ctx) -> TypeId {
  return ctx.GetBitType();
}

// Make an integral constant with specified value and type.
auto MakeIntegralConst(int64_t value, TypeId type) -> Constant {
  IntegralConstant ic;
  ic.value.push_back(static_cast<uint64_t>(value));
  return Constant{.type = type, .value = std::move(ic)};
}

// Check if an index has any X/Z bits. Returns 2-state 1-bit bool.
// For 2-state types, returns constant 1 (always known).
auto EmitIndexIsKnown(
    mir::Operand index, TypeId index_type, MirBuilder& builder)
    -> mir::Operand {
  Context& ctx = builder.GetContext();
  const Type& type = (*ctx.type_arena)[index_type];
  TypeId bool_type = GetBoolType(ctx);

  if (type.Kind() == TypeKind::kIntegral && !type.AsIntegral().is_four_state) {
    // 2-state type: always known
    return mir::Operand::Const(MakeIntegralConst(1, bool_type));
  }
  // 4-state type: emit IsKnown check
  return builder.EmitUnary(mir::UnaryOp::kIsKnown, index, bool_type);
}

// Compute validity predicate: in_bounds AND index_is_known.
// Uses signed comparisons for signed index types (kIntegral or kPackedArray).
auto EmitValidityCheck(
    mir::Operand index, TypeId index_type, int64_t lower, int64_t upper,
    MirBuilder& builder) -> mir::Operand {
  const Context& ctx = builder.GetContext();
  const Type& idx_type = (*ctx.type_arena)[index_type];
  bool is_signed =
      IsPacked(idx_type) && IsPackedSigned(idx_type, *ctx.type_arena);
  TypeId bool_type = GetBoolType(ctx);

  // Emit constants at index type
  auto lower_const = mir::Operand::Const(MakeIntegralConst(lower, index_type));
  auto upper_const = mir::Operand::Const(MakeIntegralConst(upper, index_type));

  // Bounds check - use signed ops for signed indices
  mir::Operand ge_lower;
  mir::Operand le_upper;
  if (is_signed) {
    ge_lower = builder.EmitBinary(
        mir::BinaryOp::kGreaterThanEqualSigned, index, lower_const, bool_type);
    le_upper = builder.EmitBinary(
        mir::BinaryOp::kLessThanEqualSigned, index, upper_const, bool_type);
  } else {
    ge_lower = builder.EmitBinary(
        mir::BinaryOp::kGreaterThanEqual, index, lower_const, bool_type);
    le_upper = builder.EmitBinary(
        mir::BinaryOp::kLessThanEqual, index, upper_const, bool_type);
  }
  auto in_bounds = builder.EmitBinary(
      mir::BinaryOp::kLogicalAnd, ge_lower, le_upper, bool_type);

  // X/Z check (returns 2-state bool)
  auto is_known = EmitIndexIsKnown(index, index_type, builder);

  // valid = in_bounds && is_known
  return builder.EmitBinary(
      mir::BinaryOp::kLogicalAnd, in_bounds, is_known, bool_type);
}

// Compute bit offset for packed array element select.
// Returns {offset, valid} pair.
// Uses 32-bit offset type for intermediate calculations to avoid truncation
// when index type is narrower than element_width.
auto EmitPackedElementOffset(
    mir::Operand index, TypeId index_type, const Type& array_type,
    MirBuilder& builder) -> std::pair<mir::Operand, mir::Operand> {
  const Context& ctx = builder.GetContext();
  const auto& packed_info = array_type.AsPackedArray();
  const auto& range = packed_info.range;
  uint32_t element_width = PackedArrayElementWidth(array_type, *ctx.type_arena);
  TypeId offset_type = ctx.GetOffsetType();

  // Compute validity (uses index_type for bounds comparison)
  auto valid = EmitValidityCheck(
      index, index_type, range.Lower(), range.Upper(), builder);

  // Compute offset based on direction
  // Descending [H:L]: offset = (index - lower) * width
  // Ascending [L:H]:  offset = (upper - index) * width
  // Use 32-bit offset_type for all arithmetic to avoid truncation
  mir::Operand offset;
  if (range.IsDescending()) {
    auto lower_const =
        mir::Operand::Const(MakeIntegralConst(range.Lower(), offset_type));
    auto diff = builder.EmitBinary(
        mir::BinaryOp::kSubtract, index, lower_const, offset_type);
    auto width_const =
        mir::Operand::Const(MakeIntegralConst(element_width, offset_type));
    offset = builder.EmitBinary(
        mir::BinaryOp::kMultiply, diff, width_const, offset_type);
  } else {
    auto upper_const =
        mir::Operand::Const(MakeIntegralConst(range.Upper(), offset_type));
    auto diff = builder.EmitBinary(
        mir::BinaryOp::kSubtract, upper_const, index, offset_type);
    auto width_const =
        mir::Operand::Const(MakeIntegralConst(element_width, offset_type));
    offset = builder.EmitBinary(
        mir::BinaryOp::kMultiply, diff, width_const, offset_type);
  }
  return {offset, valid};
}

// Compute bit offset for bit select on kIntegral.
// Returns {offset, valid} pair.
auto EmitBitSelectOffset(
    mir::Operand index, TypeId index_type, const Type& base_type,
    MirBuilder& builder) -> std::pair<mir::Operand, mir::Operand> {
  const auto& info = base_type.AsIntegral();

  // Implicit range: [bit_width-1:0] (descending, 0-based)
  int32_t upper = static_cast<int32_t>(info.bit_width) - 1;
  int32_t lower = 0;

  // Compute validity
  auto valid = EmitValidityCheck(index, index_type, lower, upper, builder);

  // Offset = index (0-based descending)
  return {index, valid};
}

// Compute OOB default value based on element type.
auto MakeOobDefault(TypeId element_type, uint32_t width, const TypeArena& types)
    -> mir::Operand {
  const Type& type = types[element_type];
  bool is_four_state = IsPacked(type) ? IsPackedFourState(type, types)
                                      : type.AsIntegral().is_four_state;
  if (is_four_state) {
    // Return all-X value
    IntegralConstant ic;
    uint32_t num_words = (width + 63) / 64;
    ic.value.resize(num_words, 0);
    ic.x_mask.resize(num_words, ~0ULL);  // All bits unknown (X)
    return mir::Operand::Const(
        Constant{.type = element_type, .value = std::move(ic)});
  }
  // Return 0
  return mir::Operand::Const(MakeIntegralConst(0, element_type));
}

auto LowerPackedElementSelect(
    const hir::PackedElementSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  mir::Operand base_operand = LowerExpression(data.base, builder);
  mir::Operand index_operand = LowerExpression(data.index, builder);

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];
  uint32_t element_width = PackedArrayElementWidth(base_type, *ctx.type_arena);

  // Compute offset and validity
  auto [offset, valid] = EmitPackedElementOffset(
      index_operand, index_expr.type, base_type, builder);

  // Get base as Place
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  // Create BitRangeProjection (address-only)
  mir::PlaceId slice_place = ctx.mir_arena->DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = element_width,
                          .element_type = expr.type}});

  // Read the value (may read garbage if invalid - Select handles it)
  mir::Operand extracted = mir::Operand::Use(slice_place);

  // Compute OOB default
  mir::Operand invalid_default =
      MakeOobDefault(expr.type, element_width, *ctx.type_arena);

  // Select: valid ? extracted : invalid_default
  return builder.EmitSelect(valid, extracted, invalid_default, expr.type);
}

auto LowerBitSelect(
    const hir::BitSelectExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  mir::Operand base_operand = LowerExpression(data.base, builder);
  mir::Operand index_operand = LowerExpression(data.index, builder);

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute offset and validity
  auto [offset, valid] =
      EmitBitSelectOffset(index_operand, index_expr.type, base_type, builder);

  // Get base as Place
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  // Create BitRangeProjection (width=1)
  mir::PlaceId slice_place = ctx.mir_arena->DerivePlace(
      base_place,
      mir::Projection{
          .info = mir::BitRangeProjection{
              .bit_offset = offset, .width = 1, .element_type = expr.type}});

  mir::Operand extracted = mir::Operand::Use(slice_place);

  // Compute OOB default (1-bit)
  mir::Operand invalid_default = MakeOobDefault(expr.type, 1, *ctx.type_arena);

  return builder.EmitSelect(valid, extracted, invalid_default, expr.type);
}

auto LowerRangeSelect(
    const hir::RangeSelectExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // Lower base expression
  mir::Operand base_operand = LowerExpression(data.base, builder);

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute width from select bounds
  int32_t select_lower = std::min(data.left, data.right);
  int32_t select_upper = std::max(data.left, data.right);
  auto width = static_cast<uint32_t>(select_upper - select_lower + 1);

  // Compute bit offset based on base type and direction
  // Physical bit layout: bit at Lower() is at physical position 0
  // Descending [H:L]: offset = select_lower - L
  // Ascending [L:H]: offset = H - select_upper (higher indices at lower
  // positions)
  int32_t bit_offset = 0;
  if (base_type.Kind() == TypeKind::kIntegral) {
    // kIntegral has implicit descending [bit_width-1:0]
    bit_offset = select_lower;
  } else if (base_type.Kind() == TypeKind::kPackedArray) {
    const auto& packed = base_type.AsPackedArray();
    if (packed.range.IsDescending()) {
      bit_offset = select_lower - packed.range.Lower();
    } else {
      // Ascending: higher logical index = lower physical position
      bit_offset = packed.range.Upper() - select_upper;
    }
  } else {
    throw common::InternalError(
        "LowerRangeSelect", "base must be kIntegral or kPackedArray");
  }

  // Use 32-bit offset_type for consistency with packed element select
  mir::Operand offset =
      mir::Operand::Const(MakeIntegralConst(bit_offset, ctx.GetOffsetType()));

  // Get base as Place
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  // Create BitRangeProjection (address-only)
  mir::PlaceId slice_place = ctx.mir_arena->DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = width,
                          .element_type = expr.type}});

  // Range select with constant bounds is always valid - no Select wrapper
  // needed
  return mir::Operand::Use(slice_place);
}

}  // namespace

auto LowerExpression(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::Operand {
  const hir::Expression& expr = (*builder.GetContext().hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> mir::Operand {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::ConstantExpressionData>) {
          return LowerConstant(data, builder);
        } else if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return LowerNameRef(data, builder);
        } else if constexpr (std::is_same_v<T, hir::UnaryExpressionData>) {
          return LowerUnary(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::BinaryExpressionData>) {
          return LowerBinary(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::CastExpressionData>) {
          return LowerCast(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::SystemCallExpressionData>) {
          return LowerSystemCall(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ConditionalExpressionData>) {
          return LowerConditional(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::AssignmentExpressionData>) {
          return LowerAssignment(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          LvalueResult lv = LowerLvalue(expr_id, builder);
          return mir::Operand::Use(lv.place);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          LvalueResult lv = LowerLvalue(expr_id, builder);
          return mir::Operand::Use(lv.place);
        } else if constexpr (std::is_same_v<
                                 T, hir::StructLiteralExpressionData>) {
          return LowerStructLiteral(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ArrayLiteralExpressionData>) {
          return LowerArrayLiteral(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::CallExpressionData>) {
          return LowerCall(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::NewArrayExpressionData>) {
          return LowerNewArray(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::BuiltinMethodCallExpressionData>) {
          return LowerBuiltinMethodCall(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedElementSelectExpressionData>) {
          return LowerPackedElementSelect(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::BitSelectExpressionData>) {
          return LowerBitSelect(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::RangeSelectExpressionData>) {
          return LowerRangeSelect(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerExpression", "unhandled expression kind");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
