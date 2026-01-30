#include "lyra/lowering/hir_to_mir/expression.hpp"

#include <algorithm>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/math_fn.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
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
    case hir::BinaryOp::kCaseZMatch:
      return mir::BinaryOp::kCaseZMatch;
    case hir::BinaryOp::kCaseXMatch:
      return mir::BinaryOp::kCaseXMatch;
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

  // Only packed types (kIntegral, kPackedArray) have signed/unsigned variants
  if (!IsPacked(lhs_type) || !IsPacked(rhs_type)) {
    return mir_op;
  }

  bool lhs_signed = IsPackedSigned(lhs_type, *ctx.type_arena);
  bool rhs_signed = IsPackedSigned(rhs_type, *ctx.type_arena);
  if (lhs_signed != rhs_signed) {
    throw common::InternalError(
        "SelectComparisonOp",
        "operand signedness mismatch - missing conversion");
  }

  return lhs_signed ? ToSignedVariant(mir_op) : mir_op;
}

// Select the MIR operator for division/modulo ops.
// Signed/unsigned variants only apply to packed types.
auto SelectDivModOp(const hir::BinaryExpressionData& data, const Context& ctx)
    -> mir::BinaryOp {
  const auto& lhs_type = (*ctx.type_arena)[(*ctx.hir_arena)[data.lhs].type];
  const auto& rhs_type = (*ctx.type_arena)[(*ctx.hir_arena)[data.rhs].type];

  auto mir_op = MapBinaryOp(data.op);

  // Only packed types (kIntegral, kPackedArray) have signed/unsigned variants
  if (!IsPacked(lhs_type) || !IsPacked(rhs_type)) {
    return mir_op;
  }

  bool lhs_signed = IsPackedSigned(lhs_type, *ctx.type_arena);
  bool rhs_signed = IsPackedSigned(rhs_type, *ctx.type_arena);
  if (lhs_signed != rhs_signed) {
    throw common::InternalError(
        "SelectDivModOp", "operand signedness mismatch - missing conversion");
  }

  return lhs_signed ? ToSignedVariant(mir_op) : mir_op;
}

auto IsIncrementOrDecrement(hir::UnaryOp op) -> bool {
  using UO = hir::UnaryOp;
  return op == UO::kPreincrement || op == UO::kPostincrement ||
         op == UO::kPredecrement || op == UO::kPostdecrement;
}

// Make an integral constant with specified value and type.
auto MakeIntegralConst(int64_t value, TypeId type) -> Constant {
  IntegralConstant ic;
  ic.value.push_back(static_cast<uint64_t>(value));
  ic.unknown.push_back(0);
  return Constant{.type = type, .value = std::move(ic)};
}

// Desugar increment/decrement operators into explicit read-modify-write.
// These operators have side effects (modify the operand) and return a value
// (pre returns new value, post returns old value). MIR's Place/Operand
// separation doesn't allow side-effecting Rvalues, so we desugar here.
auto LowerIncrementDecrement(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // 1. Lower operand as lvalue
  Result<LvalueResult> target_result = LowerLvalue(data.operand, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = *target_result;

  // 2. Type validation (defensive - Slang already checked)
  const Type& type = (*ctx.type_arena)[expr.type];
  if (!IsPacked(type)) {
    throw common::InternalError(
        "LowerIncrementDecrement", "operand must be packed type");
  }

  // 3. Read old value with OOB handling using GuardedUse
  mir::Operand read_val;
  if (target.IsAlwaysValid()) {
    read_val = mir::Operand::Use(target.place);
  } else {
    // OOB/X/Z index: GuardedUse returns X (4-state) or 0 (2-state)
    read_val = builder.EmitGuardedUse(target.validity, target.place, expr.type);
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
  Constant one = MakeIntegralConst(1, expr.type);

  mir::Rvalue compute_rvalue{
      .operands = {mir::Operand::Use(old_value), mir::Operand::Const(one)},
      .info = mir::BinaryRvalueInfo{.op = bin_op},
  };
  mir::PlaceId new_value =
      builder.EmitTemp(expr.type, std::move(compute_rvalue));

  // 5. Write back to target with guarded assign for OOB safety
  if (target.IsAlwaysValid()) {
    builder.EmitAssign(target.place, mir::Operand::Use(new_value));
  } else {
    builder.EmitGuardedAssign(
        target.place, mir::Operand::Use(new_value), target.validity);
  }

  // 6. Return appropriate value
  bool is_pre =
      (data.op == hir::UnaryOp::kPreincrement ||
       data.op == hir::UnaryOp::kPredecrement);
  return mir::Operand::Use(is_pre ? new_value : old_value);
}

// Lower compound assignment (+=, -=, etc.) into read-modify-write sequence.
// Similar to LowerIncrementDecrement but uses the user-provided operand.
auto LowerCompoundAssignment(
    const hir::CompoundAssignmentExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // 1. Lower target as lvalue (evaluates index expressions once)
  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = *target_result;

  // 2. Lower the RHS operand
  Result<mir::Operand> rhs_result = LowerExpression(data.operand, builder);
  if (!rhs_result) return std::unexpected(rhs_result.error());
  mir::Operand rhs = *rhs_result;

  // 3. Read old value with OOB handling using GuardedUse
  mir::Operand read_val;
  if (target.IsAlwaysValid()) {
    read_val = mir::Operand::Use(target.place);
  } else {
    // OOB/X/Z index: GuardedUse returns X (4-state) or 0 (2-state)
    read_val = builder.EmitGuardedUse(target.validity, target.place, expr.type);
  }
  mir::PlaceId old_value = ctx.AllocTemp(expr.type);
  builder.EmitAssign(old_value, read_val);

  // 4. Select the appropriate MIR operator (handle div/mod signedness)
  // Use operand types for signedness check, mirroring SelectDivModOp logic
  mir::BinaryOp mir_op = MapBinaryOp(data.op);
  if (IsDivisionOrModuloOp(data.op)) {
    const hir::Expression& operand_expr = (*ctx.hir_arena)[data.operand];
    const Type& lhs_type = (*ctx.type_arena)[expr.type];  // target type
    const Type& rhs_type = (*ctx.type_arena)[operand_expr.type];

    if (IsPacked(lhs_type) && IsPacked(rhs_type)) {
      bool lhs_signed = IsPackedSigned(lhs_type, *ctx.type_arena);
      bool rhs_signed = IsPackedSigned(rhs_type, *ctx.type_arena);
      if (lhs_signed != rhs_signed) {
        throw common::InternalError(
            "LowerCompoundAssignment",
            "operand signedness mismatch - missing conversion");
      }
      if (lhs_signed) {
        mir_op = ToSignedVariant(mir_op);
      }
    }
  }

  // 5. Compute new value: old_value op rhs
  mir::Rvalue compute_rvalue{
      .operands = {mir::Operand::Use(old_value), rhs},
      .info = mir::BinaryRvalueInfo{.op = mir_op},
  };
  mir::PlaceId new_value =
      builder.EmitTemp(expr.type, std::move(compute_rvalue));

  // 6. Write back to target with guarded assign for OOB safety
  if (target.IsAlwaysValid()) {
    builder.EmitAssign(target.place, mir::Operand::Use(new_value));
  } else {
    builder.EmitGuardedAssign(
        target.place, mir::Operand::Use(new_value), target.validity);
  }

  // 7. Return new value (compound assignment yields the new value)
  return mir::Operand::Use(new_value);
}

auto LowerUnary(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  // Desugar increment/decrement operators into read-modify-write sequence
  if (IsIncrementOrDecrement(data.op)) {
    return LowerIncrementDecrement(data, expr, builder);
  }

  Result<mir::Operand> operand_result = LowerExpression(data.operand, builder);
  if (!operand_result) return std::unexpected(operand_result.error());
  mir::Operand operand = *operand_result;

  mir::Rvalue rvalue{
      .operands = {operand},
      .info = mir::UnaryRvalueInfo{.op = MapUnaryOp(data.op)},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerBinary(
    const hir::BinaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Result<mir::Operand> lhs_result = LowerExpression(data.lhs, builder);
  if (!lhs_result) return std::unexpected(lhs_result.error());
  mir::Operand lhs = *lhs_result;

  Result<mir::Operand> rhs_result = LowerExpression(data.rhs, builder);
  if (!rhs_result) return std::unexpected(rhs_result.error());
  mir::Operand rhs = *rhs_result;

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
    MirBuilder& builder) -> Result<mir::Operand> {
  // Invariant: caller dispatched based on CastExpressionData
  if (expr.kind != hir::ExpressionKind::kCast) {
    throw common::InternalError("LowerCast", "expected kCast expression kind");
  }

  // Pattern-match: Cast(string ← bit[N]){ Concat{ all string literals } }
  // Slang types all-literal string concat as bit[N], wraps in Conversion.
  // Rewrite directly to Concat(string) to avoid the LLVM backend's packed path.
  // Handles both direct string constants and single-element concats wrapping
  // string constants (produced by replication expansion).
  const Context& ctx = builder.GetContext();
  const Type& target_ty = (*ctx.type_arena)[expr.type];
  if (target_ty.Kind() == TypeKind::kString) {
    const hir::Expression& inner = (*ctx.hir_arena)[data.operand];
    const auto* concat_data =
        std::get_if<hir::ConcatExpressionData>(&inner.data);
    if (concat_data != nullptr) {
      // Try to extract StringConstant from a concat operand expression.
      // Handles: direct ConstantExpr, or single-element ConcatExpr wrapping
      // one.
      auto try_get_string =
          [&](hir::ExpressionId eid) -> const StringConstant* {
        const hir::Expression& e = (*ctx.hir_arena)[eid];
        // Direct constant?
        if (const auto* cd =
                std::get_if<hir::ConstantExpressionData>(&e.data)) {
          const Constant& c = (*ctx.constant_arena)[cd->constant];
          return std::get_if<StringConstant>(&c.value);
        }
        // Single-element concat wrapping a constant? (replication expansion)
        if (const auto* nested =
                std::get_if<hir::ConcatExpressionData>(&e.data)) {
          if (nested->operands.size() == 1) {
            const hir::Expression& inner_e =
                (*ctx.hir_arena)[nested->operands[0]];
            if (const auto* cd =
                    std::get_if<hir::ConstantExpressionData>(&inner_e.data)) {
              const Constant& c = (*ctx.constant_arena)[cd->constant];
              return std::get_if<StringConstant>(&c.value);
            }
          }
        }
        return nullptr;
      };

      // Check all operands resolve to string literals
      bool all_string_literals = true;
      for (hir::ExpressionId op_id : concat_data->operands) {
        if (try_get_string(op_id) == nullptr) {
          all_string_literals = false;
          break;
        }
      }

      if (all_string_literals) {
        // Rewrite: emit Concat(string) with string-typed operands
        std::vector<mir::Operand> operands;
        operands.reserve(concat_data->operands.size());
        for (hir::ExpressionId op_id : concat_data->operands) {
          const auto* str = try_get_string(op_id);
          Constant string_const{.type = expr.type, .value = *str};
          operands.push_back(mir::Operand::Const(string_const));
        }

        mir::Rvalue rvalue{
            .operands = std::move(operands),
            .info = mir::ConcatRvalueInfo{.result_type = expr.type},
        };
        return mir::Operand::Use(
            builder.EmitTemp(expr.type, std::move(rvalue)));
      }
    }
  }

  // Lower operand first. Note: current lowering is structure-preserving (no
  // rewriting), so we can safely read source type from HIR after lowering.
  Result<mir::Operand> operand_result = LowerExpression(data.operand, builder);
  if (!operand_result) return std::unexpected(operand_result.error());
  mir::Operand operand = *operand_result;

  const hir::Expression& operand_expr = (*ctx.hir_arena)[data.operand];
  TypeId source_type = operand_expr.type;
  TypeId target_type = expr.type;

  // Note: 4-state -> 2-state converts X/Z to 0 (lossy but well-defined)
  // Note: 2-state -> 4-state is lossless (no X/Z bits introduced)

  mir::Rvalue rvalue{
      .operands = {operand},
      .info =
          mir::CastRvalueInfo{
              .source_type = source_type, .target_type = target_type},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerBitCast(
    const hir::BitCastExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  // Lower operand
  Result<mir::Operand> operand_result = LowerExpression(data.operand, builder);
  if (!operand_result) return std::unexpected(operand_result.error());
  mir::Operand operand = *operand_result;

  const Context& ctx = builder.GetContext();
  const hir::Expression& operand_expr = (*ctx.hir_arena)[data.operand];
  TypeId source_type = operand_expr.type;
  TypeId target_type = expr.type;

  mir::Rvalue rvalue{
      .operands = {operand},
      .info =
          mir::BitCastRvalueInfo{
              .source_type = source_type, .target_type = target_type},
  };

  mir::PlaceId temp_id = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp_id);
}

auto LowerSystemCall(
    const hir::SystemCallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  // Handle SFormatSystemCallData in expression context ($sformatf)
  if (const auto* sformat = std::get_if<hir::SFormatSystemCallData>(&data)) {
    Context& ctx = builder.GetContext();

    // Build SFormat rvalue (same logic as statement.cpp's BuildSFormatRvalue)
    mir::SFormatRvalueInfo info;
    info.default_format = sformat->default_format;

    mir::Rvalue rvalue;
    if (!sformat->ops.empty()) {
      // Compile-time path
      info.ops.reserve(sformat->ops.size());
      for (const auto& hir_op : sformat->ops) {
        if (hir_op.kind == FormatKind::kLiteral) {
          info.ops.push_back(
              mir::FormatOp{
                  .kind = FormatKind::kLiteral,
                  .value = std::nullopt,
                  .literal = hir_op.literal,
                  .type = TypeId{},
                  .mods = {},
                  .module_timeunit_power = hir_op.module_timeunit_power});
        } else {
          Result<mir::Operand> operand_result =
              LowerExpression(*hir_op.value, builder);
          if (!operand_result) return std::unexpected(operand_result.error());
          mir::Operand operand = *operand_result;
          const hir::Expression& val_expr = (*ctx.hir_arena)[*hir_op.value];
          info.ops.push_back(
              mir::FormatOp{
                  .kind = hir_op.kind,
                  .value = std::move(operand),
                  .literal = {},
                  .type = val_expr.type,
                  .mods = hir_op.mods,
                  .module_timeunit_power = hir_op.module_timeunit_power});
        }
      }
      rvalue = mir::Rvalue{.operands = {}, .info = std::move(info)};
    } else {
      // Runtime path
      info.has_runtime_format = !sformat->args.empty();
      std::vector<mir::Operand> operands;
      operands.reserve(sformat->args.size());
      for (hir::ExpressionId arg_id : sformat->args) {
        Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
        if (!arg_result) return std::unexpected(arg_result.error());
        operands.push_back(*arg_result);
      }
      rvalue =
          mir::Rvalue{.operands = std::move(operands), .info = std::move(info)};
    }

    mir::PlaceId tmp = builder.EmitTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $test$plusargs → TestPlusargsRvalueInfo (pure, no side effects)
  if (const auto* test_pa = std::get_if<hir::TestPlusargsData>(&data)) {
    Result<mir::Operand> query_result =
        LowerExpression(test_pa->query, builder);
    if (!query_result) return std::unexpected(query_result.error());
    mir::Operand query_op = *query_result;
    mir::Rvalue rvalue{
        .operands = {query_op},
        .info = mir::TestPlusargsRvalueInfo{},
    };
    mir::PlaceId tmp = builder.EmitTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $value$plusargs → ValuePlusargs statement (has side effects)
  if (const auto* val_pa = std::get_if<hir::ValuePlusargsData>(&data)) {
    Result<mir::Operand> format_result =
        LowerExpression(val_pa->format, builder);
    if (!format_result) return std::unexpected(format_result.error());
    mir::Operand format_op = *format_result;
    Result<LvalueResult> output_lv_result =
        LowerLvalue(val_pa->output, builder);
    if (!output_lv_result) return std::unexpected(output_lv_result.error());
    LvalueResult output_lv = *output_lv_result;

    Context& ctx = builder.GetContext();
    const hir::Expression& out_expr = (*ctx.hir_arena)[val_pa->output];
    TypeId output_type = out_expr.type;

    // Emit ValuePlusargs statement - returns success boolean
    return builder.EmitValuePlusargs(
        format_op, output_lv.place, output_type, expr.type);
  }

  // $fopen → SystemTfRvalueInfo
  if (const auto* fopen_data = std::get_if<hir::FopenData>(&data)) {
    Result<mir::Operand> filename_result =
        LowerExpression(fopen_data->filename, builder);
    if (!filename_result) return std::unexpected(filename_result.error());
    mir::Operand filename_op = *filename_result;
    std::vector<mir::Operand> operands = {filename_op};
    if (fopen_data->mode) {
      Result<mir::Operand> mode_result =
          LowerExpression(*fopen_data->mode, builder);
      if (!mode_result) return std::unexpected(mode_result.error());
      operands.push_back(*mode_result);
    }
    mir::Rvalue rvalue{
        .operands = std::move(operands),
        .info = mir::SystemTfRvalueInfo{.opcode = SystemTfOpcode::kFopen}};
    mir::PlaceId tmp = builder.EmitTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $time etc. -> RuntimeQueryRvalueInfo
  if (const auto* query = std::get_if<hir::RuntimeQueryData>(&data)) {
    mir::Rvalue rvalue{
        .operands = {},
        .info = mir::RuntimeQueryRvalueInfo{.kind = query->kind},
    };
    mir::PlaceId tmp = builder.EmitTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // Effect system calls ($display, etc.) are handled in statement.cpp.
  throw common::InternalError(
      "LowerSystemCall",
      "system call used in value context (only effect calls supported)");
}

auto LowerAssignment(
    const hir::AssignmentExpressionData& data, const hir::Expression& /*expr*/,
    MirBuilder& builder) -> Result<mir::Operand> {
  // Lower target as lvalue
  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = *target_result;

  // Lower value as rvalue
  Result<mir::Operand> value_result = LowerExpression(data.value, builder);
  if (!value_result) return std::unexpected(value_result.error());
  mir::Operand value = *value_result;

  if (data.is_non_blocking) {
    // Non-blocking: schedule for NBA region (validity handled in LLVM backend)
    builder.EmitDeferredAssign(target.place, value);
  } else if (target.IsAlwaysValid()) {
    builder.EmitAssign(target.place, value);
  } else {
    // Guarded assign: only write if guard is true (OOB/X/Z = no-op)
    builder.EmitGuardedAssign(target.place, value, target.validity);
  }

  // Return the value (assignment expression yields the assigned value)
  return value;
}

// Lower ternary operator (a ? b : c) to control flow.
// We use branches instead of a select instruction to ensure short-circuit
// evaluation - only the taken arm is evaluated. This matters when arms have
// side effects or access potentially invalid places.
//
// Lowering:
//   result = _
//   branch cond -> then_bb, else_bb
//   then_bb: result = then_val; jump merge_bb
//   else_bb: result = else_val; jump merge_bb
//   merge_bb: (continue here)
auto LowerConditional(
    const hir::ConditionalExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // 1. Allocate result temp BEFORE branching (dominates both arms and merge)
  mir::PlaceId result = ctx.AllocTemp(expr.type);

  // 2. Lower condition (must not emit terminator - currently safe since
  //    logical ops use binary instructions, not control flow)
  Result<mir::Operand> cond_result = LowerExpression(data.condition, builder);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::Operand cond = *cond_result;

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
  Result<mir::Operand> then_result = LowerExpression(data.then_expr, builder);
  if (!then_result) return std::unexpected(then_result.error());
  mir::Operand then_val = *then_result;
  builder.EmitAssign(result, std::move(then_val));
  builder.EmitJump(merge_bb);

  // 6. Else block: lower else_expr HERE (short-circuit), assign, terminate
  builder.SetCurrentBlock(else_bb);
  Result<mir::Operand> else_result = LowerExpression(data.else_expr, builder);
  if (!else_result) return std::unexpected(else_result.error());
  mir::Operand else_val = *else_result;
  builder.EmitAssign(result, std::move(else_val));
  builder.EmitJump(merge_bb);

  // 7. Continue in merge block
  builder.SetCurrentBlock(merge_bb);
  return mir::Operand::Use(result);
}

auto LowerStructLiteral(
    const hir::StructLiteralExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();
  const Type& type_info = (*ctx.type_arena)[expr.type];

  std::vector<mir::Operand> operands;
  operands.reserve(data.field_values.size());
  for (hir::ExpressionId field_id : data.field_values) {
    Result<mir::Operand> field_result = LowerExpression(field_id, builder);
    if (!field_result) return std::unexpected(field_result.error());
    operands.push_back(*field_result);
  }

  if (type_info.Kind() == TypeKind::kPackedStruct) {
    // Packed struct: concat fields (declaration order = MSB first)
    // result_type is the packed struct type, preserving signedness
    mir::Rvalue rvalue{
        .operands = std::move(operands),
        .info = mir::ConcatRvalueInfo{.result_type = expr.type},
    };
    return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
  }

  // Unpacked struct: aggregate construction
  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::AggregateRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
}

auto LowerCall(
    const hir::CallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Resolve mir::FunctionId from symbol (throws if not found)
  mir::FunctionId callee = ctx.ResolveCallee(data.callee);

  // Validate against the frozen signature
  const mir::FunctionSignature& sig = builder.GetArena()[callee].signature;
  if (data.arguments.size() != sig.params.size()) {
    throw common::InternalError(
        "LowerCall", "argument count mismatch with frozen signature");
  }

  // Lower arguments
  std::vector<mir::Operand> args;
  args.reserve(data.arguments.size());
  for (hir::ExpressionId arg_id : data.arguments) {
    Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
    if (!arg_result) return std::unexpected(arg_result.error());
    args.push_back(*arg_result);
  }

  // Emit Call instruction (returns Use of result or Poison for void)
  return builder.EmitCall(callee, std::move(args), expr.type);
}

auto LowerNewArray(
    const hir::NewArrayExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  std::vector<mir::Operand> operands;
  Result<mir::Operand> size_result = LowerExpression(data.size_expr, builder);
  if (!size_result) return std::unexpected(size_result.error());
  operands.push_back(*size_result);
  if (data.init_expr) {
    Result<mir::Operand> init_result =
        LowerExpression(*data.init_expr, builder);
    if (!init_result) return std::unexpected(init_result.error());
    operands.push_back(*init_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info =
          mir::BuiltinCallRvalueInfo{
              .method = mir::BuiltinMethod::kNewArray,
              .result_type = expr.type,
              .receiver = std::nullopt,
              .enum_type = std::nullopt},
  };

  mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp);
}

auto LowerArrayLiteral(
    const hir::ArrayLiteralExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  std::vector<mir::Operand> operands;
  operands.reserve(data.elements.size());
  for (hir::ExpressionId elem_id : data.elements) {
    Result<mir::Operand> elem_result = LowerExpression(elem_id, builder);
    if (!elem_result) return std::unexpected(elem_result.error());
    operands.push_back(*elem_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::AggregateRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
}

auto LowerBuiltinMethodCall(
    const hir::BuiltinMethodCallExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();
  const hir::Expression& receiver_expr = (*ctx.hir_arena)[data.receiver];

  switch (data.method) {
    case hir::BuiltinMethod::kSize: {
      Result<mir::Operand> receiver_result =
          LowerExpression(data.receiver, builder);
      if (!receiver_result) return std::unexpected(receiver_result.error());
      mir::Operand receiver = *receiver_result;
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
                  .receiver = std::nullopt,
                  .enum_type = std::nullopt},
      };
      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kPopBack:
    case hir::BuiltinMethod::kPopFront: {
      // Pop methods mutate the receiver AND return a value
      // Need receiver as PlaceId for mutation
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = *lv_result;

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kPopBack)
                                      ? mir::BuiltinMethod::kQueuePopBack
                                      : mir::BuiltinMethod::kQueuePopFront;

      return builder.EmitBuiltinCall(method, lv.place, {}, expr.type);
    }

    case hir::BuiltinMethod::kPushBack:
    case hir::BuiltinMethod::kPushFront: {
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = *lv_result;
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
        if (!arg_result) return std::unexpected(arg_result.error());
        operands.push_back(*arg_result);
      }

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kPushBack)
                                      ? mir::BuiltinMethod::kQueuePushBack
                                      : mir::BuiltinMethod::kQueuePushFront;

      return builder.EmitBuiltinCall(
          method, lv.place, std::move(operands), expr.type);
    }

    case hir::BuiltinMethod::kInsert: {
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = *lv_result;
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
        if (!arg_result) return std::unexpected(arg_result.error());
        operands.push_back(*arg_result);
      }

      return builder.EmitBuiltinCall(
          mir::BuiltinMethod::kQueueInsert, lv.place, std::move(operands),
          expr.type);
    }

    case hir::BuiltinMethod::kDelete: {
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = *lv_result;
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
        if (!arg_result) return std::unexpected(arg_result.error());
        operands.push_back(*arg_result);
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

      return builder.EmitBuiltinCall(
          method, lv.place, std::move(operands), expr.type);
    }

    case hir::BuiltinMethod::kEnumNext:
    case hir::BuiltinMethod::kEnumPrev: {
      // operands[0] = receiver value
      Result<mir::Operand> receiver_result =
          LowerExpression(data.receiver, builder);
      if (!receiver_result) return std::unexpected(receiver_result.error());
      mir::Operand receiver_val = *receiver_result;
      std::vector<mir::Operand> operands = {receiver_val};

      // operands[1] = optional step N
      if (!data.args.empty()) {
        Result<mir::Operand> step_result =
            LowerExpression(data.args[0], builder);
        if (!step_result) return std::unexpected(step_result.error());
        operands.push_back(*step_result);
      }

      // Get enum type from receiver
      TypeId enum_type_id = receiver_expr.type;
      const Type& enum_type = (*ctx.type_arena)[enum_type_id];
      if (enum_type.Kind() != TypeKind::kEnum) {
        throw common::InternalError(
            "LowerBuiltinMethodCall", "enum method on non-enum type");
      }

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kEnumNext)
                                      ? mir::BuiltinMethod::kEnumNext
                                      : mir::BuiltinMethod::kEnumPrev;

      mir::Rvalue rvalue{
          .operands = std::move(operands),
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = method,
                  .result_type = expr.type,
                  .receiver = std::nullopt,
                  .enum_type = enum_type_id},
      };

      mir::PlaceId temp = builder.EmitTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kEnumName: {
      // operands[0] = receiver value
      Result<mir::Operand> receiver_result =
          LowerExpression(data.receiver, builder);
      if (!receiver_result) return std::unexpected(receiver_result.error());
      mir::Operand receiver_val = *receiver_result;

      // Get enum type from receiver
      TypeId enum_type_id = receiver_expr.type;
      const Type& enum_type = (*ctx.type_arena)[enum_type_id];
      if (enum_type.Kind() != TypeKind::kEnum) {
        throw common::InternalError(
            "LowerBuiltinMethodCall", "enum method on non-enum type");
      }

      mir::Rvalue rvalue{
          .operands = {receiver_val},
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = mir::BuiltinMethod::kEnumName,
                  .result_type = expr.type,
                  .receiver = std::nullopt,
                  .enum_type = enum_type_id},
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

// Check if index type is 4-state (has X/Z bits).
auto IsFourStateIndex(TypeId index_type, const TypeArena& types) -> bool {
  const Type& type = types[index_type];
  if (IsPacked(type)) {
    return IsPackedFourState(type, types);
  }
  return false;
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

  // Compute validity using IndexValidity rvalue
  bool check_known = IsFourStateIndex(index_type, *ctx.type_arena);
  auto valid = builder.EmitIndexValidity(
      index, range.Lower(), range.Upper(), check_known);

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

// Compute bit offset for bit select on packed types with implicit range.
// kIntegral and kPackedStruct have implicit descending [bit_width-1:0].
// Returns {offset, valid} pair.
auto EmitBitSelectOffset(
    mir::Operand index, TypeId index_type, const Type& base_type,
    MirBuilder& builder) -> std::pair<mir::Operand, mir::Operand> {
  const Context& ctx = builder.GetContext();

  // Get bit width based on type kind
  uint32_t bit_width = 0;
  switch (base_type.Kind()) {
    case TypeKind::kIntegral:
      bit_width = base_type.AsIntegral().bit_width;
      break;
    case TypeKind::kPackedStruct:
      bit_width = base_type.AsPackedStruct().total_bit_width;
      break;
    default:
      throw common::InternalError(
          "EmitBitSelectOffset", "base must be kIntegral or kPackedStruct");
  }

  // Implicit range: [bit_width-1:0] (descending, 0-based)
  int32_t upper = static_cast<int32_t>(bit_width) - 1;
  int32_t lower = 0;

  // Compute validity using IndexValidity rvalue
  bool check_known = IsFourStateIndex(index_type, *ctx.type_arena);
  auto valid = builder.EmitIndexValidity(index, lower, upper, check_known);

  // Offset = index (0-based descending)
  return {index, valid};
}

auto LowerPackedElementSelect(
    const hir::PackedElementSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  Result<mir::Operand> base_result = LowerExpression(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  Result<mir::Operand> index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;

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

  // Use GuardedUse for OOB-safe read: valid ? Use(place) : oob_default
  return builder.EmitGuardedUse(valid, slice_place, expr.type);
}

auto LowerBitSelect(
    const hir::BitSelectExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  Result<mir::Operand> base_result = LowerExpression(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  Result<mir::Operand> index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;

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

  // Use GuardedUse for OOB-safe read: valid ? Use(place) : oob_default
  return builder.EmitGuardedUse(valid, slice_place, expr.type);
}

auto LowerRangeSelect(
    const hir::RangeSelectExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base expression
  Result<mir::Operand> base_result = LowerExpression(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

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
  switch (base_type.Kind()) {
    case TypeKind::kIntegral:
    case TypeKind::kPackedStruct:
      // kIntegral and kPackedStruct have implicit descending [bit_width-1:0]
      bit_offset = select_lower;
      break;
    case TypeKind::kPackedArray: {
      const auto& packed = base_type.AsPackedArray();
      if (packed.range.IsDescending()) {
        bit_offset = select_lower - packed.range.Lower();
      } else {
        // Ascending: higher logical index = lower physical position
        bit_offset = packed.range.Upper() - select_upper;
      }
      break;
    }
    default:
      throw common::InternalError(
          "LowerRangeSelect",
          "non-packed base type should have been rejected at AST->HIR");
  }

  // Use 32-bit offset_type for consistency with packed element select
  mir::Operand offset =
      mir::Operand::Const(MakeIntegralConst(bit_offset, ctx.GetOffsetType()));

  // Get base as Place
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  if (bit_offset < 0 || static_cast<uint32_t>(bit_offset) + width >
                            PackedBitWidth(base_type, *ctx.type_arena)) {
    throw common::InternalError(
        "LowerRangeSelect", "bit_offset + width exceeds base bit width");
  }

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

// Compute offset and validity for indexed part-select.
// For ascending (+:): select bits [index .. index + width - 1]
// For descending (-:): select bits [index - width + 1 .. index]
// Returns {offset, valid} pair.
auto EmitIndexedPartSelectOffset(
    mir::Operand index, TypeId index_type, const Type& base_type,
    uint32_t width, bool ascending, MirBuilder& builder)
    -> std::pair<mir::Operand, mir::Operand> {
  const Context& ctx = builder.GetContext();
  TypeId offset_type = ctx.GetOffsetType();

  // Determine base range bounds
  int32_t lower = 0;
  int32_t upper = 0;
  bool base_descending = true;  // Default for kIntegral

  if (base_type.Kind() == TypeKind::kIntegral) {
    const auto& info = base_type.AsIntegral();
    lower = 0;
    upper = static_cast<int32_t>(info.bit_width) - 1;
    base_descending = true;
  } else if (base_type.Kind() == TypeKind::kPackedArray) {
    const auto& packed = base_type.AsPackedArray();
    lower = packed.range.Lower();
    upper = packed.range.Upper();
    base_descending = packed.range.IsDescending();
  } else if (base_type.Kind() == TypeKind::kPackedStruct) {
    const auto& info = base_type.AsPackedStruct();
    lower = 0;
    upper = static_cast<int32_t>(info.total_bit_width) - 1;
    base_descending = true;
  } else {
    throw common::InternalError(
        "EmitIndexedPartSelectOffset",
        "base must be kIntegral, kPackedArray, or kPackedStruct");
  }

  // Compute effective bounds for validity check using int64_t to avoid
  // overflow:
  // +: (ascending): valid index range is [lower, upper - width + 1]
  // -: (descending): valid index range is [lower + width - 1, upper]
  int64_t eff_lower_64 =
      ascending ? lower : (static_cast<int64_t>(lower) + width - 1);
  int64_t eff_upper_64 =
      ascending ? (static_cast<int64_t>(upper) - width + 1) : upper;

  mir::Operand valid;
  if (eff_lower_64 > eff_upper_64 || eff_lower_64 < INT32_MIN ||
      eff_lower_64 > INT32_MAX || eff_upper_64 < INT32_MIN ||
      eff_upper_64 > INT32_MAX) {
    // Width exceeds base range - always invalid
    TypeId bit_type = ctx.GetBitType();
    valid = mir::Operand::Const(MakeIntegralConst(0, bit_type));
  } else {
    bool check_known = IsFourStateIndex(index_type, *ctx.type_arena);
    valid = builder.EmitIndexValidity(
        index, static_cast<int32_t>(eff_lower_64),
        static_cast<int32_t>(eff_upper_64), check_known);
  }

  // Compute physical bit offset based on base direction and part-select
  // direction Physical layout: bit at Lower() is at physical position 0
  //
  // Base descending [H:L]:
  //   +: offset = index - L
  //   -: offset = (index - width + 1) - L = index - width + 1 - L
  //
  // Base ascending [L:H]:
  //   +: offset = H - (index + width - 1) = H - index - width + 1
  //   -: offset = H - index
  //
  // kIntegral (implicit [W-1:0] descending):
  //   +: offset = index
  //   -: offset = index - width + 1
  //
  // Use int64_t for intermediate calculations to avoid overflow
  mir::Operand offset;
  if (base_descending) {
    if (ascending) {
      // offset = index - lower
      auto lower_const =
          mir::Operand::Const(MakeIntegralConst(lower, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, lower_const, offset_type);
    } else {
      // offset = index - width + 1 - lower = index - (lower + width - 1)
      int64_t adjust = static_cast<int64_t>(lower) + width - 1;
      auto adjust_const =
          mir::Operand::Const(MakeIntegralConst(adjust, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, adjust_const, offset_type);
    }
  } else {
    // Ascending base
    if (ascending) {
      // offset = upper - index - width + 1 = (upper - width + 1) - index
      int64_t adjust = static_cast<int64_t>(upper) - width + 1;
      auto adjust_const =
          mir::Operand::Const(MakeIntegralConst(adjust, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, adjust_const, index, offset_type);
    } else {
      // offset = upper - index
      auto upper_const =
          mir::Operand::Const(MakeIntegralConst(upper, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, upper_const, index, offset_type);
    }
  }

  return {offset, valid};
}

auto LowerIndexedPartSelect(
    const hir::IndexedPartSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  Result<mir::Operand> base_result = LowerExpression(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  Result<mir::Operand> index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute offset and validity
  auto [offset, valid] = EmitIndexedPartSelectOffset(
      index_operand, index_expr.type, base_type, data.width, data.ascending,
      builder);

  // Get base as Place
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  // Create BitRangeProjection
  mir::PlaceId slice_place = ctx.mir_arena->DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = data.width,
                          .element_type = expr.type}});

  // Use GuardedUse for OOB-safe read: valid ? Use(place) : oob_default
  return builder.EmitGuardedUse(valid, slice_place, expr.type);
}

auto LowerPackedFieldAccess(
    const hir::PackedFieldAccessExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base expression
  Result<mir::Operand> base_result = LowerExpression(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  // Get base expression type
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];

  // Create constant offset operand
  mir::Operand offset = mir::Operand::Const(
      MakeIntegralConst(data.bit_offset, ctx.GetOffsetType()));

  // Get base as Place
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  const Type& base_type = (*ctx.type_arena)[base_expr.type];
  if (data.bit_offset + data.bit_width >
      PackedBitWidth(base_type, *ctx.type_arena)) {
    throw common::InternalError(
        "LowerPackedFieldAccess",
        "bit_offset + bit_width exceeds base bit width");
  }

  // Create BitRangeProjection (address-only)
  mir::PlaceId slice_place = ctx.mir_arena->DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = data.bit_width,
                          .element_type = expr.type}});

  // Packed field access with constant offset is always valid
  return mir::Operand::Use(slice_place);
}

auto LowerConcat(
    const hir::ConcatExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  std::vector<mir::Operand> operands;
  operands.reserve(data.operands.size());
  for (hir::ExpressionId op_id : data.operands) {
    Result<mir::Operand> op_result = LowerExpression(op_id, builder);
    if (!op_result) return std::unexpected(op_result.error());
    operands.push_back(*op_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::ConcatRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
}

auto LowerMathCall(
    const hir::MathCallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder) -> Result<mir::Operand> {
  int expected_arity = GetMathFnArity(data.fn);
  if (std::cmp_not_equal(data.args.size(), expected_arity)) {
    throw common::InternalError(
        "LowerMathCall",
        std::format(
            "arity mismatch for {}: expected {}, got {}", ToString(data.fn),
            expected_arity, data.args.size()));
  }

  std::vector<mir::Operand> operands;
  operands.reserve(data.args.size());
  for (hir::ExpressionId arg_id : data.args) {
    Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
    if (!arg_result) return std::unexpected(arg_result.error());
    operands.push_back(*arg_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::MathCallRvalueInfo{.fn = data.fn},
  };
  return mir::Operand::Use(builder.EmitTemp(expr.type, std::move(rvalue)));
}

auto LowerElementAccessRvalue(
    const hir::ElementAccessExpressionData& data, MirBuilder& builder)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  Result<mir::Operand> base_result = LowerExpression(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  Result<mir::Operand> index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;

  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  mir::PlaceId base_place =
      GetOrMaterializePlace(base_operand, base_expr.type, builder);

  const Type& base_type = (*ctx.type_arena)[base_expr.type];
  if (base_type.Kind() != TypeKind::kUnpackedArray &&
      base_type.Kind() != TypeKind::kDynamicArray &&
      base_type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "LowerElementAccessRvalue", "base is not an array or queue");
  }

  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  index_operand = NormalizeUnpackedIndex(
      index_operand, index_expr.type, base_type, builder);

  mir::Projection proj{
      .info = mir::IndexProjection{.index = index_operand},
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base_place, std::move(proj));

  return mir::Operand::Use(result_place);
}

}  // namespace

auto LowerExpression(hir::ExpressionId expr_id, MirBuilder& builder)
    -> Result<mir::Operand> {
  const hir::Expression& expr = (*builder.GetContext().hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> Result<mir::Operand> {
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
        } else if constexpr (std::is_same_v<T, hir::BitCastExpressionData>) {
          return LowerBitCast(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::SystemCallExpressionData>) {
          return LowerSystemCall(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ConditionalExpressionData>) {
          return LowerConditional(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::AssignmentExpressionData>) {
          return LowerAssignment(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::CompoundAssignmentExpressionData>) {
          return LowerCompoundAssignment(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          // TODO(hankhsu): Use GuardedUse for OOB-safe reads
          return LowerElementAccessRvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          Context& ctx = builder.GetContext();
          const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
          if (const auto* lit = std::get_if<hir::StructLiteralExpressionData>(
                  &base_expr.data)) {
            // Struct literal field access — extract directly without lvalue.
            // field_index and field_values are both in declaration order.
            if (data.field_index < 0 || static_cast<size_t>(data.field_index) >=
                                            lit->field_values.size()) {
              throw common::InternalError(
                  "MemberAccessExpression",
                  "field_index out of range for struct literal");
            }
            return LowerExpression(
                lit->field_values[data.field_index], builder);
          }
          // Addressable base — use existing lvalue+load path
          Result<LvalueResult> lv_result = LowerLvalue(expr_id, builder);
          if (!lv_result) return std::unexpected(lv_result.error());
          LvalueResult lv = *lv_result;
          return mir::Operand::Use(lv.place);
        } else if constexpr (std::is_same_v<
                                 T, hir::UnionMemberAccessExpressionData>) {
          // Union member access - always valid (member index is compile-time)
          Result<LvalueResult> lv_result = LowerLvalue(expr_id, builder);
          if (!lv_result) return std::unexpected(lv_result.error());
          LvalueResult lv = *lv_result;
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
        } else if constexpr (std::is_same_v<
                                 T, hir::IndexedPartSelectExpressionData>) {
          return LowerIndexedPartSelect(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedFieldAccessExpressionData>) {
          return LowerPackedFieldAccess(data, expr, builder);
        } else if constexpr (std::is_same_v<T, hir::ConcatExpressionData>) {
          return LowerConcat(data, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::HierarchicalRefExpressionData>) {
          mir::PlaceId place_id = builder.GetContext().LookupPlace(data.target);
          return mir::Operand::Use(place_id);
        } else if constexpr (std::is_same_v<T, hir::MathCallExpressionData>) {
          return LowerMathCall(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerExpression", "unhandled expression kind");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
