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

// Desugar increment/decrement operators into explicit read-modify-write.
// These operators have side effects (modify the operand) and return a value
// (pre returns new value, post returns old value). MIR's Place/Operand
// separation doesn't allow side-effecting Rvalues, so we desugar here.
auto LowerIncrementDecrement(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> mir::Operand {
  Context& ctx = builder.GetContext();

  // 1. Lower operand as lvalue - guaranteed PlaceId
  //    Currently only supports NameRef; projections are future work.
  mir::PlaceId target_place = LowerLvalue(data.operand, builder);

  // 2. Type validation (defensive - Slang already checked)
  const Type& type = (*ctx.type_arena)[expr.type];
  if (type.Kind() != TypeKind::kIntegral) {
    throw common::InternalError(
        "LowerIncrementDecrement", "operand must be integral type");
  }

  // 3. Save old value
  mir::PlaceId old_value = ctx.AllocTemp(expr.type);
  builder.EmitAssign(old_value, mir::Operand::Use(target_place));

  // 4. Compute new value using existing binary op machinery
  bool is_increment =
      (data.op == hir::UnaryOp::kPreincrement ||
       data.op == hir::UnaryOp::kPostincrement);
  mir::BinaryOp bin_op =
      is_increment ? mir::BinaryOp::kAdd : mir::BinaryOp::kSubtract;

  // Create typed constant 1 - type is needed for interpreter to determine width
  Constant one = MakeOne(expr.type);

  mir::Rvalue compute_rvalue{
      .kind = mir::RvalueKind::kBinary,
      .op = static_cast<int>(bin_op),
      .operands = {mir::Operand::Use(old_value), mir::Operand::Const(one)},
      .info = {},
  };
  mir::PlaceId new_value =
      builder.EmitTemp(expr.type, std::move(compute_rvalue));

  // 5. Write back to target
  builder.EmitAssign(target_place, mir::Operand::Use(new_value));

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
      .kind = mir::RvalueKind::kUnary,
      .op = static_cast<int>(MapUnaryOp(data.op)),
      .operands = {operand},
      .info = {},
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
      .kind = mir::RvalueKind::kBinary,
      .op = static_cast<int>(mir_op),
      .operands = {lhs, rhs},
      .info = {},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

// Sentinel value for Rvalue::op when opcode is not applicable (e.g., kCast).
constexpr int kUnusedOp = 0;

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
  if (src.Kind() != TypeKind::kIntegral) {
    throw common::InternalError(
        "LowerCast",
        "non-integral source should have been rejected in AST->HIR");
  }
  if (tgt.Kind() != TypeKind::kIntegral) {
    throw common::InternalError(
        "LowerCast",
        "non-integral target should have been rejected in AST->HIR");
  }

  const auto& src_int = src.AsIntegral();

  if (src_int.is_four_state) {
    throw common::InternalError(
        "LowerCast", "4-state source should have been rejected in AST->HIR");
  }
  // Note: 4-state target is allowed when source is 2-state (lossless
  // conversion)

  mir::Rvalue rvalue{
      .kind = mir::RvalueKind::kCast,
      .op = kUnusedOp,
      .operands = {operand},
      .info =
          mir::CastInfo{.source_type = source_type, .target_type = target_type},
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
  // 1. Lower target as lvalue
  mir::PlaceId target = LowerLvalue(data.target, builder);

  // 2. Lower value as rvalue
  mir::Operand value = LowerExpression(data.value, builder);

  // 3. Emit assignment
  builder.EmitAssign(target, value);

  // 4. Return the value (assignment expression yields the assigned value)
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
      .kind = mir::RvalueKind::kAggregate,
      .op = kUnusedOp,
      .operands = std::move(operands),
      .info = mir::AggregateInfo{.result_type = expr.type},
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
      .kind = mir::RvalueKind::kCall,
      .op = kUnusedOp,
      .operands = std::move(args),
      .info = mir::UserCallInfo{.callee = callee},
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
      .kind = mir::RvalueKind::kBuiltinCall,
      .op = kUnusedOp,
      .operands = std::move(operands),
      .info =
          mir::BuiltinCallInfo{
              .method = mir::BuiltinMethod::kNewArray,
              .result_type = expr.type},
  };

  mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp);
}

auto LowerBuiltinMethodCall(
    const hir::BuiltinMethodCallExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> mir::Operand {
  switch (data.method) {
    case hir::BuiltinMethod::kSize: {
      mir::Operand receiver = LowerExpression(data.receiver, builder);
      mir::Rvalue rvalue{
          .kind = mir::RvalueKind::kBuiltinCall,
          .op = kUnusedOp,
          .operands = {receiver},
          .info =
              mir::BuiltinCallInfo{
                  .method = mir::BuiltinMethod::kArraySize,
                  .result_type = expr.type},
      };
      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }
    case hir::BuiltinMethod::kDelete:
      throw common::InternalError(
          "LowerBuiltinMethodCall",
          "delete() in expression context should have been rejected");
  }
  throw common::InternalError(
      "LowerBuiltinMethodCall", "unknown builtin method");
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
          mir::PlaceId place = LowerLvalue(expr_id, builder);
          return mir::Operand::Use(place);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          mir::PlaceId place = LowerLvalue(expr_id, builder);
          return mir::Operand::Use(place);
        } else if constexpr (std::is_same_v<
                                 T, hir::StructLiteralExpressionData>) {
          return LowerStructLiteral(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::CallExpressionData>) {
          return LowerCall(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::NewArrayExpressionData>) {
          return LowerNewArray(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::BuiltinMethodCallExpressionData>) {
          return LowerBuiltinMethodCall(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerExpression", "unhandled expression kind");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
