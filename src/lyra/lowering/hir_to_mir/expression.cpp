#include "lyra/lowering/hir_to_mir/expression.hpp"

#include <algorithm>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <functional>
#include <initializer_list>
#include <optional>
#include <tuple>
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
#include "lyra/common/system_function.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/materialize_cache.hpp"
#include "lyra/lowering/hir_to_mir/packed_alignment.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/call.hpp"
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
// result into a temp via EmitPlaceTemp, then return Use(temp).
//
// This avoids value identity issues (MIR has no SSA) and keeps the lowering
// uniform. Future expression kinds (casts, selects, bit-slices) should
// follow the same pattern.

namespace {

// Forward declaration for recursive lowering with cache.
// All internal helpers call this; the public LowerExpression creates the cache.
auto LowerExpressionImpl(
    hir::ExpressionId expr_id, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand>;

// When a sub-expression creates new blocks, previously lowered operands in
// in_args must be rethreaded to the current block. Used by normal call
// lowering.
void RethreadArgsIfBlockChanged(
    MirBuilder& builder, BlockIndex before, std::vector<mir::Operand>& args) {
  if (builder.CurrentBlock() != before) {
    for (auto& op : args) {
      op = builder.ThreadValueToCurrentBlock(op);
    }
  }
}

// Build the copy-in operand for an inout DPI argument from its lvalue place.
// For inout, we need to read the current value before the call and pass it
// as the copy-in. The write-back destination is the same place.
// This is a semantic read step, not a raw pointer alias.
auto BuildDpiInoutCopyIn(mir::PlaceId place, MirBuilder& builder)
    -> mir::Operand {
  const auto& p = builder.GetArena()[place];
  if (!mir::IsReadableRoot(p.root.kind)) {
    throw common::InternalError(
        "BuildDpiInoutCopyIn",
        std::format(
            "DPI inout copy-in requires a readable place root, got {}",
            static_cast<int>(p.root.kind)));
  }
  return mir::Operand::Use(place);
}

// Rethread input_value operands in existing DPI arg bindings after a block
// change. Same purpose as RethreadArgsIfBlockChanged but for DpiArgBinding.
void RethreadDpiBindingsIfBlockChanged(
    MirBuilder& builder, BlockIndex before,
    std::vector<mir::DpiArgBinding>& bindings) {
  if (builder.CurrentBlock() != before) {
    for (auto& binding : bindings) {
      if (binding.input_value) {
        binding.input_value =
            builder.ThreadValueToCurrentBlock(*binding.input_value);
      }
    }
  }
}

// Fix up UseTemp operands that became invalid after a block change.
// When a sub-expression (e.g., ternary) creates new blocks, previously
// lowered operands may refer to temps defined in a prior block.
// This threads those values through block params so they're available
// in the current block.
void FixupOperandsAfterBlockChange(
    MirBuilder& builder, BlockIndex before,
    std::initializer_list<mir::Operand*> operands) {
  if (builder.CurrentBlock() == before) return;
  for (mir::Operand* op : operands) {
    *op = builder.ThreadValueToCurrentBlock(*op);
  }
}

// Emit OOB default fill for a non-packed place that contains 4-state content.
// Recursively walks the type structure and assigns all-X to packed 4-state
// leaves. 2-state leaves and handles retain their zero-initialized default.
void EmitOOBDefault(mir::PlaceId place, TypeId type_id, MirBuilder& builder) {
  const Context& ctx = builder.GetContext();
  const TypeArena& types = *ctx.type_arena;
  const Type& type = types[type_id];

  if (IsPacked(type)) {
    if (IsIntrinsicallyPackedFourState(type, types)) {
      uint32_t bw = PackedBitWidth(type, types);
      IntegralConstant ic;
      if (bw <= 64) {
        ic.value.push_back(0);
        ic.unknown.push_back(bw == 64 ? ~0ULL : (1ULL << bw) - 1);
      } else {
        // Multi-limb: fill all limbs with unknown bits.
        uint32_t full_limbs = bw / 64;
        uint32_t remaining = bw % 64;
        for (uint32_t i = 0; i < full_limbs; ++i) {
          ic.value.push_back(0);
          ic.unknown.push_back(~0ULL);
        }
        if (remaining > 0) {
          ic.value.push_back(0);
          ic.unknown.push_back((1ULL << remaining) - 1);
        }
      }
      auto constant = Constant{.type = type_id, .value = std::move(ic)};
      builder.EmitAssign(place, mir::Operand::Const(std::move(constant)));
    }
    return;
  }

  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      for (int i = 0; std::cmp_less(i, info.fields.size()); ++i) {
        if (!IsIntrinsicallyFourState(info.fields[i].type, types)) continue;
        mir::PlaceId field_place = ctx.DerivePlace(
            place,
            mir::Projection{.info = mir::FieldProjection{.field_index = i}});
        EmitOOBDefault(field_place, info.fields[i].type, builder);
      }
      break;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      if (!IsIntrinsicallyFourState(info.element_type, types)) break;
      TypeId offset_type = ctx.GetOffsetType();
      for (int64_t idx = 0; std::cmp_less(idx, info.range.Size()); ++idx) {
        mir::Operand idx_op = mir::Operand::Const(
            Constant{
                .type = offset_type,
                .value = IntegralConstant{
                    .value = {static_cast<uint64_t>(idx)}, .unknown = {0}}});
        mir::PlaceId elem_place = ctx.DerivePlace(
            place,
            mir::Projection{.info = mir::IndexProjection{.index = idx_op}});
        EmitOOBDefault(elem_place, info.element_type, builder);
      }
      break;
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      if (!info.storage_is_four_state) break;
      // Union storage is a single blob; fill via its first 4-state member.
      for (uint32_t i = 0; i < info.members.size(); ++i) {
        if (!IsIntrinsicallyFourState(info.members[i].type, types)) continue;
        mir::PlaceId member_place = ctx.DerivePlace(
            place, mir::Projection{
                       .info = mir::UnionMemberProjection{.member_index = i}});
        EmitOOBDefault(member_place, info.members[i].type, builder);
        break;
      }
      break;
    }
    default:
      break;
  }
}

auto LowerConstant(const hir::ConstantExpressionData& data, MirBuilder& builder)
    -> mir::Operand {
  const Constant& constant =
      (*builder.GetContext().active_constant_arena)[data.constant];
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
  LvalueResult target = std::move(*target_result);

  // 2. Type validation (defensive - Slang already checked)
  const Type& type = (*ctx.type_arena)[expr.type];
  if (!IsPacked(type)) {
    throw common::InternalError(
        "LowerIncrementDecrement", "operand must be packed type");
  }

  // 3. Read old value with OOB handling using GuardedUse
  mir::PlaceId target_place = target.GetLocalPlace();
  mir::Operand read_val;
  if (target.IsAlwaysValid()) {
    read_val = mir::Operand::Use(target_place);
  } else {
    // OOB/X/Z index: GuardedUse returns X (4-state) or 0 (2-state)
    read_val = builder.EmitGuardedUse(target.validity, target_place, expr.type);
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
      builder.EmitPlaceTemp(expr.type, std::move(compute_rvalue));

  // 5. Write back to target with guarded assign for OOB safety
  mir::WriteTarget dest = target.dest;
  if (target.IsAlwaysValid()) {
    builder.EmitAssign(dest, mir::Operand::Use(new_value));
  } else {
    builder.EmitGuardedAssign(
        dest, mir::Operand::Use(new_value), target.validity);
  }

  // Writeback for AA compound access (aa[k]++)
  if (target.writeback) {
    auto wb = std::move(target.writeback);
    builder.EmitAssocOp(
        mir::AssocOp{
            .receiver = wb->aa_place,
            .data = mir::AssocSet{
                .key = wb->key, .value = mir::Operand::Use(wb->temp_place)}});
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
    const hir::Expression& expr, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // 1. Lower target as lvalue (evaluates index expressions once, separate
  // scope)
  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = std::move(*target_result);

  // 2. Lower the RHS operand
  Result<mir::Operand> rhs_result =
      LowerExpressionImpl(data.operand, builder, cache);
  if (!rhs_result) return std::unexpected(rhs_result.error());
  mir::Operand rhs = *rhs_result;

  // 3. Read old value with OOB handling using GuardedUse
  mir::PlaceId target_place = target.GetLocalPlace();
  mir::Operand read_val;
  if (target.IsAlwaysValid()) {
    read_val = mir::Operand::Use(target_place);
  } else {
    // OOB/X/Z index: GuardedUse returns X (4-state) or 0 (2-state)
    read_val = builder.EmitGuardedUse(target.validity, target_place, expr.type);
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
      builder.EmitPlaceTemp(expr.type, std::move(compute_rvalue));

  // 6. Write back to target with guarded assign for OOB safety
  mir::WriteTarget dest = target.dest;
  if (target.IsAlwaysValid()) {
    builder.EmitAssign(dest, mir::Operand::Use(new_value));
  } else {
    builder.EmitGuardedAssign(
        dest, mir::Operand::Use(new_value), target.validity);
  }

  // Writeback for AA compound access (aa[k] += v)
  if (target.writeback) {
    auto wb = std::move(target.writeback);
    builder.EmitAssocOp(
        mir::AssocOp{
            .receiver = wb->aa_place,
            .data = mir::AssocSet{
                .key = wb->key, .value = mir::Operand::Use(wb->temp_place)}});
  }

  // 7. Return new value (compound assignment yields the new value)
  return mir::Operand::Use(new_value);
}

auto LowerUnary(
    const hir::UnaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  // Desugar increment/decrement operators into read-modify-write sequence
  // Note: LowerIncrementDecrement uses LowerLvalue, which has separate scope
  if (IsIncrementOrDecrement(data.op)) {
    return LowerIncrementDecrement(data, expr, builder);
  }

  Result<mir::Operand> operand_result =
      LowerExpressionImpl(data.operand, builder, cache);
  if (!operand_result) return std::unexpected(operand_result.error());
  mir::Operand operand = *operand_result;

  mir::Rvalue rvalue{
      .operands = {operand},
      .info = mir::UnaryRvalueInfo{.op = MapUnaryOp(data.op)},
  };

  return builder.EmitValueTemp(expr.type, std::move(rvalue));
}

auto LowerBinary(
    const hir::BinaryExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Result<mir::Operand> lhs_result =
      LowerExpressionImpl(data.lhs, builder, cache);
  if (!lhs_result) return std::unexpected(lhs_result.error());
  mir::Operand lhs = *lhs_result;

  BlockIndex before_rhs = builder.CurrentBlock();
  Result<mir::Operand> rhs_result =
      LowerExpressionImpl(data.rhs, builder, cache);
  if (!rhs_result) return std::unexpected(rhs_result.error());
  mir::Operand rhs = *rhs_result;
  FixupOperandsAfterBlockChange(builder, before_rhs, {&lhs});

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

  return builder.EmitBinary(mir_op, lhs, rhs, expr.type);
}

auto LowerCast(
    const hir::CastExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  // Invariant: caller dispatched based on CastExpressionData
  if (expr.kind != hir::ExpressionKind::kCast) {
    throw common::InternalError("LowerCast", "expected kCast expression kind");
  }

  // Pattern-match: Cast(string <- bit[N]){ Concat{ all string literals } }
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
          const Constant& c = (*ctx.active_constant_arena)[cd->constant];
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
              const Constant& c = (*ctx.active_constant_arena)[cd->constant];
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
            builder.EmitPlaceTemp(expr.type, std::move(rvalue)));
      }
    }
  }

  // Lower operand first. Note: current lowering is structure-preserving (no
  // rewriting), so we can safely read source type from HIR after lowering.
  Result<mir::Operand> operand_result =
      LowerExpressionImpl(data.operand, builder, cache);
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

  return builder.EmitValueTemp(expr.type, std::move(rvalue));
}

auto LowerBitCast(
    const hir::BitCastExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  // Lower operand
  Result<mir::Operand> operand_result =
      LowerExpressionImpl(data.operand, builder, cache);
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

  return builder.EmitValueTemp(expr.type, std::move(rvalue));
}

auto LowerSystemCall(
    const hir::SystemCallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
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
              LowerExpressionImpl(*hir_op.value, builder, cache);
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
        Result<mir::Operand> arg_result =
            LowerExpressionImpl(arg_id, builder, cache);
        if (!arg_result) return std::unexpected(arg_result.error());
        operands.push_back(*arg_result);
      }
      rvalue =
          mir::Rvalue{.operands = std::move(operands), .info = std::move(info)};
    }

    mir::PlaceId tmp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $test$plusargs -> TestPlusargsRvalueInfo (pure, no side effects)
  if (const auto* test_pa = std::get_if<hir::TestPlusargsData>(&data)) {
    Context& ctx = builder.GetContext();
    Result<mir::Operand> query_result =
        LowerExpressionImpl(test_pa->query, builder, cache);
    if (!query_result) return std::unexpected(query_result.error());
    mir::Operand query_op = *query_result;
    const hir::Expression& query_expr = (*ctx.hir_arena)[test_pa->query];
    mir::Rvalue rvalue{
        .operands = {},
        .info =
            mir::TestPlusargsRvalueInfo{
                .query =
                    mir::TypedOperand{
                        .operand = std::move(query_op),
                        .type = query_expr.type}},
    };
    mir::PlaceId tmp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $value$plusargs -> unified Call with SystemTfOpcode
  // Note: LowerLvalue has separate cache scope
  if (const auto* val_pa = std::get_if<hir::ValuePlusargsData>(&data)) {
    Result<mir::Operand> format_result =
        LowerExpressionImpl(val_pa->format, builder, cache);
    if (!format_result) return std::unexpected(format_result.error());
    mir::Operand format_op = *format_result;
    Result<LvalueResult> output_lv_result =
        LowerLvalue(val_pa->output, builder);
    if (!output_lv_result) return std::unexpected(output_lv_result.error());
    LvalueResult output_lv = std::move(*output_lv_result);

    Context& ctx = builder.GetContext();
    const hir::Expression& out_expr = (*ctx.hir_arena)[val_pa->output];
    TypeId output_type = out_expr.type;

    // Emit unified Call - returns success boolean via staging temp
    return builder.EmitSystemTfCallExpr(
        SystemTfOpcode::kValuePlusargs, {format_op}, expr.type,
        {{output_lv.GetLocalPlace(), output_type, mir::PassMode::kOut}});
  }

  // $fopen -> FopenRvalueInfo with semantic fields for string coercion
  if (const auto* fopen_data = std::get_if<hir::FopenData>(&data)) {
    Context& ctx = builder.GetContext();
    Result<mir::Operand> filename_result =
        LowerExpressionImpl(fopen_data->filename, builder, cache);
    if (!filename_result) return std::unexpected(filename_result.error());
    const hir::Expression& filename_expr =
        (*ctx.hir_arena)[fopen_data->filename];

    mir::FopenRvalueInfo info{
        .filename =
            mir::TypedOperand{
                .operand = std::move(*filename_result),
                .type = filename_expr.type},
        .mode = std::nullopt,
    };

    if (fopen_data->mode) {
      Result<mir::Operand> mode_result =
          LowerExpressionImpl(*fopen_data->mode, builder, cache);
      if (!mode_result) return std::unexpected(mode_result.error());
      const hir::Expression& mode_expr = (*ctx.hir_arena)[*fopen_data->mode];
      info.mode = mir::TypedOperand{
          .operand = std::move(*mode_result), .type = mode_expr.type};
    }

    mir::Rvalue rvalue{.operands = {}, .info = std::move(info)};
    mir::PlaceId tmp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $time etc. -> RuntimeQueryRvalueInfo
  if (const auto* query = std::get_if<hir::RuntimeQueryData>(&data)) {
    mir::Rvalue rvalue{
        .operands = {},
        .info = mir::RuntimeQueryRvalueInfo{.kind = query->kind},
    };
    mir::PlaceId tmp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $random/$urandom -> SystemTfRvalueInfo (stateful, not pure queries)
  if (const auto* random = std::get_if<hir::RandomData>(&data)) {
    SystemTfOpcode opcode = (random->kind == RandomKind::kRandom)
                                ? SystemTfOpcode::kRandom
                                : SystemTfOpcode::kUrandom;
    mir::Rvalue rvalue{
        .operands = {},
        .info = mir::SystemTfRvalueInfo{.opcode = opcode},
    };
    mir::PlaceId tmp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
    return mir::Operand::Use(tmp);
  }

  // $fgetc -> SystemTfRvalueInfo (reads file state)
  if (const auto* fgetc = std::get_if<hir::FgetcData>(&data)) {
    Result<mir::Operand> desc_result =
        LowerExpression(fgetc->descriptor, builder);
    if (!desc_result) return std::unexpected(desc_result.error());

    mir::Rvalue rvalue{
        .operands = {*desc_result},
        .info = mir::SystemTfRvalueInfo{.opcode = SystemTfOpcode::kFgetc},
    };
    return builder.EmitValueTemp(expr.type, std::move(rvalue));
  }

  // $ungetc -> SystemTfRvalueInfo (modifies file state)
  if (const auto* ungetc = std::get_if<hir::UngetcData>(&data)) {
    Result<mir::Operand> char_result =
        LowerExpression(ungetc->character, builder);
    if (!char_result) return std::unexpected(char_result.error());
    mir::Operand char_op = *char_result;

    BlockIndex before_desc = builder.CurrentBlock();
    Result<mir::Operand> desc_result =
        LowerExpression(ungetc->descriptor, builder);
    if (!desc_result) return std::unexpected(desc_result.error());
    FixupOperandsAfterBlockChange(builder, before_desc, {&char_op});

    mir::Rvalue rvalue{
        .operands = {char_op, *desc_result},
        .info = mir::SystemTfRvalueInfo{.opcode = SystemTfOpcode::kUngetc},
    };
    return builder.EmitValueTemp(expr.type, std::move(rvalue));
  }

  // $fgets -> unified Call with SystemTfOpcode (reads file, writes to string)
  if (const auto* fgets_data = std::get_if<hir::FgetsData>(&data)) {
    Result<mir::Operand> desc_result =
        LowerExpression(fgets_data->descriptor, builder);
    if (!desc_result) return std::unexpected(desc_result.error());
    Result<LvalueResult> output_lv_result =
        LowerLvalue(fgets_data->str_output, builder);
    if (!output_lv_result) return std::unexpected(output_lv_result.error());
    LvalueResult output_lv = std::move(*output_lv_result);

    Context& ctx = builder.GetContext();
    const hir::Expression& out_expr = (*ctx.hir_arena)[fgets_data->str_output];
    TypeId output_type = out_expr.type;

    // Emit unified Call - returns char count via staging temp
    return builder.EmitSystemTfCallExpr(
        SystemTfOpcode::kFgets, {*desc_result}, expr.type,
        {{output_lv.GetLocalPlace(), output_type, mir::PassMode::kOut}});
  }

  // $fread -> unified Call with SystemTfOpcode (reads binary file)
  if (const auto* fread_data = std::get_if<hir::FreadData>(&data)) {
    Result<mir::Operand> desc_result =
        LowerExpression(fread_data->descriptor, builder);
    if (!desc_result) return std::unexpected(desc_result.error());
    Result<LvalueResult> target_lv_result =
        LowerLvalue(fread_data->target, builder);
    if (!target_lv_result) return std::unexpected(target_lv_result.error());
    LvalueResult target_lv = std::move(*target_lv_result);

    Context& ctx = builder.GetContext();
    TypeId target_type = fread_data->target_type;
    TypeId i32_type = ctx.GetOffsetType();

    // Build operands: [descriptor, target_type_width, is_memory,
    //                  start_or_-1, count_or_-1]
    // We encode is_memory and element_width as compile-time constants
    // to let the runtime know how to read data.
    const Type& ty = (*ctx.type_arena)[target_type];
    int32_t element_width = 0;
    if (fread_data->is_memory) {
      // Memory variant: get element width from array element type
      const auto& arr = ty.AsUnpackedArray();
      element_width = static_cast<int32_t>(
          PackedBitWidth((*ctx.type_arena)[arr.element_type], *ctx.type_arena));
    } else {
      // Integral variant: use type's packed bit width
      element_width = static_cast<int32_t>(PackedBitWidth(ty, *ctx.type_arena));
    }

    // Create constant operands for type info
    mir::Operand width_op =
        mir::Operand::Const(MakeIntegralConst(element_width, i32_type));
    mir::Operand is_mem_op = mir::Operand::Const(
        MakeIntegralConst(fread_data->is_memory ? 1 : 0, i32_type));

    // Optional start/count with -1 sentinel for "not specified"
    mir::Operand start_op;
    if (fread_data->start) {
      Result<mir::Operand> s = LowerExpression(*fread_data->start, builder);
      if (!s) return std::unexpected(s.error());
      start_op = *s;
    } else {
      start_op = mir::Operand::Const(MakeIntegralConst(-1, i32_type));
    }

    mir::Operand count_op;
    if (fread_data->count) {
      Result<mir::Operand> c = LowerExpression(*fread_data->count, builder);
      if (!c) return std::unexpected(c.error());
      count_op = *c;
    } else {
      count_op = mir::Operand::Const(MakeIntegralConst(-1, i32_type));
    }

    // Emit unified Call with:
    // in_args: [descriptor, element_width, is_memory, start, count]
    // writebacks: target (where data is written)
    // Memory variant uses kDirectToDest: runtime writes directly to backing
    // store via raw pointer, following the $readmemh bulk-write pattern.
    auto wb_kind = fread_data->is_memory ? mir::WritebackKind::kDirectToDest
                                         : mir::WritebackKind::kStaged;
    return builder.EmitSystemTfCallExpr(
        SystemTfOpcode::kFread,
        {*desc_result, width_op, is_mem_op, start_op, count_op}, expr.type,
        {{target_lv.GetLocalPlace(), target_type, mir::PassMode::kOut,
          wb_kind}});
  }

  // $fscanf -> unified Call with SystemTfOpcode (formatted file input)
  if (const auto* fscanf_data = std::get_if<hir::FscanfData>(&data)) {
    Result<mir::Operand> desc_result =
        LowerExpression(fscanf_data->descriptor, builder);
    if (!desc_result) return std::unexpected(desc_result.error());
    Result<mir::Operand> format_result =
        LowerExpression(fscanf_data->format, builder);
    if (!format_result) return std::unexpected(format_result.error());

    Context& ctx = builder.GetContext();

    // Lower all output lvalues and build writebacks
    std::vector<std::tuple<mir::PlaceId, TypeId, mir::PassMode>> writebacks;
    for (const auto& output : fscanf_data->outputs) {
      Result<LvalueResult> output_lv_result = LowerLvalue(output, builder);
      if (!output_lv_result) return std::unexpected(output_lv_result.error());
      const hir::Expression& out_expr = (*ctx.hir_arena)[output];
      writebacks.emplace_back(
          output_lv_result->GetLocalPlace(), out_expr.type,
          mir::PassMode::kOut);
    }

    // Emit unified Call with:
    // in_args: [descriptor, format]
    // writebacks: all output lvalues
    return builder.EmitSystemTfCallExpr(
        SystemTfOpcode::kFscanf, {*desc_result, *format_result}, expr.type,
        std::move(writebacks));
  }

  // $system -> SystemCmdRvalueInfo (side-effecting shell command)
  if (const auto* system_cmd = std::get_if<hir::SystemCmdData>(&data)) {
    Context& ctx = builder.GetContext();

    mir::SystemCmdRvalueInfo info{.command = std::nullopt};
    if (system_cmd->command) {
      Result<mir::Operand> cmd_result =
          LowerExpression(*system_cmd->command, builder);
      if (!cmd_result) {
        return std::unexpected(cmd_result.error());
      }
      const auto& cmd_expr = (*ctx.hir_arena)[*system_cmd->command];
      info.command = mir::TypedOperand{
          .operand = std::move(*cmd_result), .type = cmd_expr.type};
    }

    mir::Rvalue rvalue{.operands = {}, .info = std::move(info)};
    return builder.EmitValueTemp(expr.type, std::move(rvalue));
  }

  // Array query functions -> ArrayQueryRvalueInfo
  if (const auto* arr_query = std::get_if<hir::ArrayQueryData>(&data)) {
    Context& ctx = builder.GetContext();

    // Lower array and dim operands
    Result<mir::Operand> array_result =
        LowerExpression(arr_query->array, builder);
    if (!array_result) return std::unexpected(array_result.error());

    Result<mir::Operand> dim_result = LowerExpression(arr_query->dim, builder);
    if (!dim_result) return std::unexpected(dim_result.error());

    // Extract dimension metadata from array type
    mir::ArrayQueryRvalueInfo info{
        .kind = arr_query->kind,
        .dims = {},
        .total_dims = 0,
        .unpacked_dims = 0,
    };

    // Helper to fill DimInfo for a Lyra type
    std::function<void(TypeId, bool)> extract_dims;
    extract_dims = [&](TypeId type_id, bool is_packed) {
      const Type& type = (*ctx.type_arena)[type_id];

      switch (type.Kind()) {
        case TypeKind::kUnpackedArray: {
          if (info.total_dims >= mir::kMaxArrayQueryDims) {
            throw common::InternalError(
                "LowerSystemCall", "array has too many dimensions (max 8)");
          }
          const auto& arr = type.AsUnpackedArray();
          info.dims.at(info.total_dims) = mir::DimInfo{
              .left = arr.range.left,
              .right = arr.range.right,
              .is_variable_sized = false,
              .is_packed = false,
          };
          ++info.total_dims;
          ++info.unpacked_dims;
          extract_dims(arr.element_type, false);
          break;
        }
        case TypeKind::kDynamicArray: {
          if (info.total_dims >= mir::kMaxArrayQueryDims) {
            throw common::InternalError(
                "LowerSystemCall", "array has too many dimensions (max 8)");
          }
          const auto& dyn = type.AsDynamicArray();
          info.dims.at(info.total_dims) = mir::DimInfo{
              .left = 0,
              .right = -1,  // Sentinel for variable-sized
              .is_variable_sized = true,
              .is_packed = false,
          };
          ++info.total_dims;
          ++info.unpacked_dims;
          extract_dims(dyn.element_type, false);
          break;
        }
        case TypeKind::kQueue: {
          if (info.total_dims >= mir::kMaxArrayQueryDims) {
            throw common::InternalError(
                "LowerSystemCall", "array has too many dimensions (max 8)");
          }
          const auto& q = type.AsQueue();
          info.dims.at(info.total_dims) = mir::DimInfo{
              .left = 0,
              .right = -1,  // Sentinel for variable-sized
              .is_variable_sized = true,
              .is_packed = false,
          };
          ++info.total_dims;
          ++info.unpacked_dims;
          extract_dims(q.element_type, false);
          break;
        }
        case TypeKind::kPackedArray: {
          if (info.total_dims >= mir::kMaxArrayQueryDims) {
            throw common::InternalError(
                "LowerSystemCall", "array has too many dimensions (max 8)");
          }
          const auto& packed = type.AsPackedArray();
          info.dims.at(info.total_dims) = mir::DimInfo{
              .left = packed.range.left,
              .right = packed.range.right,
              .is_variable_sized = false,
              .is_packed = true,
          };
          ++info.total_dims;
          extract_dims(packed.element_type, true);
          break;
        }
        case TypeKind::kIntegral: {
          if (!is_packed) {
            // Integral type as element: treat as 1-dim packed [N-1:0]
            if (info.total_dims >= mir::kMaxArrayQueryDims) {
              throw common::InternalError(
                  "LowerSystemCall", "array has too many dimensions (max 8)");
            }
            const auto& integral = type.AsIntegral();
            info.dims.at(info.total_dims) = mir::DimInfo{
                .left = static_cast<int32_t>(integral.bit_width) - 1,
                .right = 0,
                .is_variable_sized = false,
                .is_packed = true,
            };
            ++info.total_dims;
          }
          break;
        }
        case TypeKind::kString:
          // String: dimension functions are NOT defined by LRM (only
          // $dimensions=1). These should be folded to 'x at compile time, not
          // reach runtime. If we somehow get here, treat string as a non-array
          // element (stop).
          break;
        default:
          // Non-array element type: stop
          break;
      }
    };

    extract_dims(arr_query->array_type, false);

    mir::Rvalue rvalue{
        .operands = {*array_result, *dim_result},
        .info = std::move(info),
    };
    return builder.EmitValueTemp(expr.type, std::move(rvalue));
  }

  // Effect system calls ($display, etc.) are handled in statement.cpp.
  throw common::InternalError(
      "LowerSystemCall",
      "system call used in value context (only effect calls supported)");
}

auto LowerAssignment(
    const hir::AssignmentExpressionData& data, const hir::Expression& /*expr*/,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Check for direct AA element assignment: aa[k] = v
  // Detect before LowerLvalue to avoid unnecessary copy-in.
  const hir::Expression& target_expr = (*ctx.hir_arena)[data.target];
  if (const auto* elem_data =
          std::get_if<hir::ElementAccessExpressionData>(&target_expr.data)) {
    const hir::Expression& base_expr = (*ctx.hir_arena)[elem_data->base];
    const Type& base_type = (*ctx.type_arena)[base_expr.type];
    if (base_type.Kind() == TypeKind::kAssociativeArray) {
      // Lower base as lvalue
      Result<LvalueResult> base_lv = LowerLvalue(elem_data->base, builder);
      if (!base_lv) return std::unexpected(base_lv.error());
      // Lower key
      Result<mir::Operand> key_result =
          LowerExpressionImpl(elem_data->index, builder, cache);
      if (!key_result) return std::unexpected(key_result.error());
      // Lower value
      Result<mir::Operand> value_result =
          LowerExpressionImpl(data.value, builder, cache);
      if (!value_result) return std::unexpected(value_result.error());
      builder.EmitAssocOp(
          mir::AssocOp{
              .receiver = base_lv->GetLocalPlace(),
              .data =
                  mir::AssocSet{.key = *key_result, .value = *value_result}});
      return *value_result;
    }
  }

  // Lower target as lvalue (separate cache scope)
  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = std::move(*target_result);

  // Lower value as rvalue
  Result<mir::Operand> value_result =
      LowerExpressionImpl(data.value, builder, cache);
  if (!value_result) return std::unexpected(value_result.error());
  mir::Operand value = *value_result;

  // B2: Use WriteTarget for assignment destination.
  mir::WriteTarget dest = target.dest;

  if (data.is_non_blocking) {
    if (target.IsAlwaysValid()) {
      builder.EmitDeferredAssign(dest, value);
    } else {
      // Guard NBA: only schedule if validity is true (OOB = no-op)
      mir::Operand cond = builder.MaterializeForBranch(target.validity);
      BlockIndex store_bb = builder.CreateBlock();
      BlockIndex merge_bb = builder.CreateBlock();
      builder.EmitBranch(cond, store_bb, merge_bb);

      builder.SetCurrentBlock(store_bb);
      value = builder.ThreadValueToCurrentBlock(std::move(value));
      builder.EmitDeferredAssign(dest, value);
      builder.EmitJump(merge_bb);

      builder.SetCurrentBlock(merge_bb);
    }
  } else if (target.IsAlwaysValid()) {
    builder.EmitAssign(dest, value);
  } else {
    // Guarded assign: only write if guard is true (OOB/X/Z = no-op)
    builder.EmitGuardedAssign(dest, value, target.validity);
  }

  // Writeback for AA compound access (aa[k].field = v)
  if (target.writeback) {
    auto wb = std::move(target.writeback);
    builder.EmitAssocOp(
        mir::AssocOp{
            .receiver = wb->aa_place,
            .data = mir::AssocSet{
                .key = wb->key, .value = mir::Operand::Use(wb->temp_place)}});
  }

  // Return the value (assignment expression yields the assigned value)
  return value;
}

// Lower ternary operator (a ? b : c) to control flow with SSA block args.
// We use branches instead of a select instruction to ensure short-circuit
// evaluation - only the taken arm is evaluated. This matters when arms have
// side effects or access potentially invalid places.
//
// SSA-by-construction lowering:
//   branch cond -> then_bb, else_bb
//   then_bb: then_val = ...; jump merge_bb(then_val)
//   else_bb: else_val = ...; jump merge_bb(else_val)
//   merge_bb(result: T): (continue here, result is SSA temp)
//
// The merge block has a single parameter (result temp_id) that receives
// values from both predecessors via edge args. This makes the temp SSA
// by construction - exactly one definition (the block param).
auto LowerConditional(
    const hir::ConditionalExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // 1. Lower condition (must not emit terminator - currently safe since
  //    logical ops use binary instructions, not control flow)
  Result<mir::Operand> cond_result =
      LowerExpressionImpl(data.condition, builder, cache);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::Operand cond = *cond_result;

  // 2. EmitBranch requires Use operand; materialize if needed
  //    Use condition's type (not expr.type) for the temp
  if (cond.kind != mir::Operand::Kind::kUse) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

  // 3. Create blocks: then, else, and merge with 1 param for the result
  BlockIndex then_bb = builder.CreateBlock();
  BlockIndex else_bb = builder.CreateBlock();
  auto [merge_bb, merge_temps] = builder.CreateBlockWithParams({expr.type});
  int result_temp_id = merge_temps[0];

  // 4. Emit branch (terminates current block)
  builder.EmitBranch(cond, then_bb, else_bb);

  // 5. Then block: lower then_expr HERE (short-circuit), jump with value
  builder.SetCurrentBlock(then_bb);
  Result<mir::Operand> then_result =
      LowerExpressionImpl(data.then_expr, builder, cache);
  if (!then_result) return std::unexpected(then_result.error());
  mir::Operand then_val = *then_result;
  // Cast to ternary result type if needed (then_expr type may differ, e.g.,
  // range select produces a structurally equivalent but distinct TypeId).
  const hir::Expression& then_expr = (*ctx.hir_arena)[data.then_expr];
  then_val = builder.EmitCast(std::move(then_val), then_expr.type, expr.type);
  builder.EmitJump(merge_bb, {std::move(then_val)});

  // 6. Else block: lower else_expr HERE (short-circuit), jump with value
  builder.SetCurrentBlock(else_bb);
  Result<mir::Operand> else_result =
      LowerExpressionImpl(data.else_expr, builder, cache);
  if (!else_result) return std::unexpected(else_result.error());
  mir::Operand else_val = *else_result;
  const hir::Expression& else_expr = (*ctx.hir_arena)[data.else_expr];
  else_val = builder.EmitCast(std::move(else_val), else_expr.type, expr.type);
  builder.EmitJump(merge_bb, {std::move(else_val)});

  // 7. Continue in merge block; result is the block param (SSA temp)
  builder.SetCurrentBlock(merge_bb);
  return mir::Operand::UseTemp(result_temp_id);
}

auto LowerStructLiteral(
    const hir::StructLiteralExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();
  const Type& type_info = (*ctx.type_arena)[expr.type];

  std::vector<mir::Operand> operands;
  operands.reserve(data.field_values.size());
  for (hir::ExpressionId field_id : data.field_values) {
    BlockIndex before = builder.CurrentBlock();
    Result<mir::Operand> field_result =
        LowerExpressionImpl(field_id, builder, cache);
    if (!field_result) return std::unexpected(field_result.error());
    if (builder.CurrentBlock() != before) {
      for (auto& op : operands) {
        op = builder.ThreadValueToCurrentBlock(op);
      }
    }
    operands.push_back(*field_result);
  }

  if (type_info.Kind() == TypeKind::kPackedStruct) {
    // Packed struct: concat fields (declaration order = MSB first)
    // result_type is the packed struct type, preserving signedness
    mir::Rvalue rvalue{
        .operands = std::move(operands),
        .info = mir::ConcatRvalueInfo{.result_type = expr.type},
    };
    return mir::Operand::Use(
        builder.EmitPlaceTemp(expr.type, std::move(rvalue)));
  }

  // Unpacked struct: aggregate construction
  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::AggregateRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitPlaceTemp(expr.type, std::move(rvalue)));
}

auto LowerDpiCall(
    const hir::CallExpressionData& data, const hir::Expression& expr,
    mir::DpiImportRef ref, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  const auto& sig = ref.signature;
  if (data.arguments.size() != sig.params.size()) {
    throw common::InternalError(
        "LowerDpiCall", "DPI argument count mismatch with frozen signature");
  }

  std::vector<mir::DpiArgBinding> bindings;
  bindings.reserve(data.arguments.size());

  for (size_t i = 0; i < data.arguments.size(); ++i) {
    const auto& param = sig.params[i];
    hir::ExpressionId arg_id = data.arguments[i];
    mir::DpiArgBinding binding;

    BlockIndex before = builder.CurrentBlock();

    switch (param.direction) {
      case ParameterDirection::kInput: {
        Result<mir::Operand> val = LowerExpressionImpl(arg_id, builder, cache);
        if (!val) return std::unexpected(val.error());
        RethreadDpiBindingsIfBlockChanged(builder, before, bindings);
        binding.input_value = *val;
        break;
      }
      case ParameterDirection::kOutput: {
        Result<LvalueResult> lv = LowerLvalue(arg_id, builder);
        if (!lv) return std::unexpected(lv.error());
        binding.writeback_dest = lv->GetLocalPlace();
        break;
      }
      case ParameterDirection::kInOut: {
        Result<LvalueResult> lv = LowerLvalue(arg_id, builder);
        if (!lv) return std::unexpected(lv.error());
        RethreadDpiBindingsIfBlockChanged(builder, before, bindings);
        binding.input_value = BuildDpiInoutCopyIn(lv->GetLocalPlace(), builder);
        binding.writeback_dest = lv->GetLocalPlace();
        break;
      }
      case ParameterDirection::kRef:
      case ParameterDirection::kConstRef:
        throw common::InternalError(
            "LowerDpiCall",
            "ref/const-ref direction should have been rejected upstream");
    }
    bindings.push_back(std::move(binding));
  }

  return builder.EmitDpiCall(std::move(ref), std::move(bindings), expr.type);
}

auto LowerCall(
    const hir::CallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Check DPI first -- separate call family.
  auto dpi_ref = ctx.ResolveDpiImport(data.callee);
  if (dpi_ref) {
    return LowerDpiCall(data, expr, std::move(*dpi_ref), builder, cache);
  }

  // Non-DPI: resolve to Callee (user function, design function, system TF).
  mir::Callee callee = ctx.ResolveCallTarget(data.callee);

  // Non-DPI: get signature for argument validation from the correct arena.
  const mir::FunctionSignature& sig = ctx.ResolveCallSignature(callee);
  if (data.arguments.size() != sig.params.size()) {
    throw common::InternalError(
        "LowerCall", "argument count mismatch with frozen signature");
  }

  // Lower arguments - handle input vs output/inout differently
  std::vector<mir::Operand> in_args;
  std::vector<mir::CallWriteback> writebacks;

  for (size_t i = 0; i < data.arguments.size(); ++i) {
    hir::ExpressionId arg_id = data.arguments[i];
    const mir::FunctionParam& param = sig.params[i];

    switch (param.kind) {
      case mir::PassingKind::kValue: {
        BlockIndex before = builder.CurrentBlock();
        Result<mir::Operand> arg_result =
            LowerExpressionImpl(arg_id, builder, cache);
        if (!arg_result) return std::unexpected(arg_result.error());
        RethreadArgsIfBlockChanged(builder, before, in_args);
        in_args.push_back(*arg_result);
        break;
      }
      case mir::PassingKind::kOut:
      case mir::PassingKind::kInOut: {
        Result<LvalueResult> lv_result = LowerLvalue(arg_id, builder);
        if (!lv_result) return std::unexpected(lv_result.error());

        mir::PassMode mode = (param.kind == mir::PassingKind::kOut)
                                 ? mir::PassMode::kOut
                                 : mir::PassMode::kInOut;
        writebacks.push_back({
            .tmp = lv_result->GetLocalPlace(),
            .dest = lv_result->GetLocalPlace(),
            .type = param.type,
            .mode = mode,
            .arg_index = static_cast<int32_t>(i),
        });
        break;
      }
      case mir::PassingKind::kRef:
      case mir::PassingKind::kConstRef:
        return std::unexpected(
            Diagnostic::Unsupported(
                expr.span,
                "ref/const ref parameters not yet supported in non-deferred "
                "calls",
                UnsupportedCategory::kFeature));
    }
  }

  return builder.EmitCallWithWritebacks(
      callee, std::move(in_args), std::move(writebacks), expr.type);
}

auto LowerNewArray(
    const hir::NewArrayExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  std::vector<mir::Operand> operands;
  Result<mir::Operand> size_result =
      LowerExpressionImpl(data.size_expr, builder, cache);
  if (!size_result) return std::unexpected(size_result.error());
  operands.push_back(*size_result);
  if (data.init_expr) {
    BlockIndex before = builder.CurrentBlock();
    Result<mir::Operand> init_result =
        LowerExpressionImpl(*data.init_expr, builder, cache);
    if (!init_result) return std::unexpected(init_result.error());
    if (builder.CurrentBlock() != before) {
      for (auto& op : operands) {
        op = builder.ThreadValueToCurrentBlock(op);
      }
    }
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

  mir::PlaceId temp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
  return mir::Operand::Use(temp);
}

auto LowerArrayLiteral(
    const hir::ArrayLiteralExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  std::vector<mir::Operand> operands;
  operands.reserve(data.elements.size());
  for (hir::ExpressionId elem_id : data.elements) {
    BlockIndex before = builder.CurrentBlock();
    Result<mir::Operand> elem_result =
        LowerExpressionImpl(elem_id, builder, cache);
    if (!elem_result) return std::unexpected(elem_result.error());
    if (builder.CurrentBlock() != before) {
      for (auto& op : operands) {
        op = builder.ThreadValueToCurrentBlock(op);
      }
    }
    operands.push_back(*elem_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::AggregateRvalueInfo{.result_type = expr.type},
  };
  return mir::Operand::Use(builder.EmitPlaceTemp(expr.type, std::move(rvalue)));
}

auto LowerBuiltinMethodCall(
    const hir::BuiltinMethodCallExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();
  const hir::Expression& receiver_expr = (*ctx.hir_arena)[data.receiver];

  switch (data.method) {
    case hir::BuiltinMethod::kSize: {
      const Type& receiver_type = (*ctx.type_arena)[receiver_expr.type];
      // AA size/num -> AssocNum
      if (receiver_type.Kind() == TypeKind::kAssociativeArray) {
        Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
        if (!lv_result) return std::unexpected(lv_result.error());
        mir::PlaceId dest = ctx.AllocTemp(expr.type);
        builder.EmitAssocOp(
            mir::AssocOp{
                .receiver = lv_result->GetLocalPlace(),
                .data = mir::AssocNum{.dest = dest}});
        return mir::Operand::Use(dest);
      }
      Result<mir::Operand> receiver_result =
          LowerExpressionImpl(data.receiver, builder, cache);
      if (!receiver_result) return std::unexpected(receiver_result.error());
      mir::Operand receiver = *receiver_result;
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
      mir::PlaceId temp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kPopBack:
    case hir::BuiltinMethod::kPopFront: {
      // Pop methods mutate the receiver AND return a value
      // Need receiver as PlaceId for mutation (LowerLvalue has separate scope)
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = std::move(*lv_result);

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kPopBack)
                                      ? mir::BuiltinMethod::kQueuePopBack
                                      : mir::BuiltinMethod::kQueuePopFront;

      return builder.EmitBuiltinCall(method, lv.GetLocalPlace(), {}, expr.type);
    }

    case hir::BuiltinMethod::kPushBack:
    case hir::BuiltinMethod::kPushFront: {
      // Note: LowerLvalue has separate cache scope
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = std::move(*lv_result);
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        BlockIndex before = builder.CurrentBlock();
        Result<mir::Operand> arg_result =
            LowerExpressionImpl(arg_id, builder, cache);
        if (!arg_result) return std::unexpected(arg_result.error());
        if (builder.CurrentBlock() != before) {
          for (auto& op : operands) {
            op = builder.ThreadValueToCurrentBlock(op);
          }
        }
        operands.push_back(*arg_result);
      }

      mir::BuiltinMethod method = (data.method == hir::BuiltinMethod::kPushBack)
                                      ? mir::BuiltinMethod::kQueuePushBack
                                      : mir::BuiltinMethod::kQueuePushFront;

      return builder.EmitBuiltinCall(
          method, lv.GetLocalPlace(), std::move(operands), expr.type);
    }

    case hir::BuiltinMethod::kInsert: {
      // Note: LowerLvalue has separate cache scope
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = std::move(*lv_result);
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        BlockIndex before = builder.CurrentBlock();
        Result<mir::Operand> arg_result =
            LowerExpressionImpl(arg_id, builder, cache);
        if (!arg_result) return std::unexpected(arg_result.error());
        if (builder.CurrentBlock() != before) {
          for (auto& op : operands) {
            op = builder.ThreadValueToCurrentBlock(op);
          }
        }
        operands.push_back(*arg_result);
      }

      return builder.EmitBuiltinCall(
          mir::BuiltinMethod::kQueueInsert, lv.GetLocalPlace(),
          std::move(operands), expr.type);
    }

    case hir::BuiltinMethod::kDelete: {
      const Type& receiver_type = (*ctx.type_arena)[receiver_expr.type];
      // AA delete: delete() or delete(key)
      if (receiver_type.Kind() == TypeKind::kAssociativeArray) {
        Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
        if (!lv_result) return std::unexpected(lv_result.error());
        if (data.args.empty()) {
          builder.EmitAssocOp(
              mir::AssocOp{
                  .receiver = lv_result->GetLocalPlace(),
                  .data = mir::AssocDelete{}});
        } else {
          Result<mir::Operand> key_result =
              LowerExpressionImpl(data.args[0], builder, cache);
          if (!key_result) return std::unexpected(key_result.error());
          builder.EmitAssocOp(
              mir::AssocOp{
                  .receiver = lv_result->GetLocalPlace(),
                  .data = mir::AssocDeleteKey{.key = *key_result}});
        }
        return mir::Operand::Poison();
      }

      // Note: LowerLvalue has separate cache scope
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      LvalueResult lv = std::move(*lv_result);
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        BlockIndex before = builder.CurrentBlock();
        Result<mir::Operand> arg_result =
            LowerExpressionImpl(arg_id, builder, cache);
        if (!arg_result) return std::unexpected(arg_result.error());
        if (builder.CurrentBlock() != before) {
          for (auto& op : operands) {
            op = builder.ThreadValueToCurrentBlock(op);
          }
        }
        operands.push_back(*arg_result);
      }

      // Determine method: kArrayDelete, kQueueDelete, or kQueueDeleteAt
      mir::BuiltinMethod method = mir::BuiltinMethod::kArrayDelete;
      if (receiver_type.Kind() == TypeKind::kDynamicArray) {
        method = mir::BuiltinMethod::kArrayDelete;
      } else if (operands.empty()) {
        method = mir::BuiltinMethod::kQueueDelete;
      } else {
        method = mir::BuiltinMethod::kQueueDeleteAt;
      }

      return builder.EmitBuiltinCall(
          method, lv.GetLocalPlace(), std::move(operands), expr.type);
    }

    case hir::BuiltinMethod::kAssocExists: {
      // receiver as lvalue (AA place), key as rvalue
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      std::vector<mir::Operand> operands;
      for (hir::ExpressionId arg_id : data.args) {
        Result<mir::Operand> arg_result =
            LowerExpressionImpl(arg_id, builder, cache);
        if (!arg_result) return std::unexpected(arg_result.error());
        operands.push_back(*arg_result);
      }
      mir::PlaceId dest = ctx.AllocTemp(expr.type);
      builder.EmitAssocOp(
          mir::AssocOp{
              .receiver = lv_result->GetLocalPlace(),
              .data = mir::AssocExists{.dest = dest, .key = operands[0]}});
      return mir::Operand::Use(dest);
    }

    case hir::BuiltinMethod::kAssocFirst:
    case hir::BuiltinMethod::kAssocLast: {
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      // key ref arg: lower as lvalue
      Result<LvalueResult> key_lv = LowerLvalue(data.args[0], builder);
      if (!key_lv) return std::unexpected(key_lv.error());
      mir::PlaceId dest = ctx.AllocTemp(expr.type);
      if (data.method == hir::BuiltinMethod::kAssocFirst) {
        builder.EmitAssocOp(
            mir::AssocOp{
                .receiver = lv_result->GetLocalPlace(),
                .data = mir::AssocIterFirst{
                    .dest_found = dest, .out_key = key_lv->GetLocalPlace()}});
      } else {
        builder.EmitAssocOp(
            mir::AssocOp{
                .receiver = lv_result->GetLocalPlace(),
                .data = mir::AssocIterLast{
                    .dest_found = dest, .out_key = key_lv->GetLocalPlace()}});
      }
      return mir::Operand::Use(dest);
    }

    case hir::BuiltinMethod::kAssocNext:
    case hir::BuiltinMethod::kAssocPrev: {
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      Result<LvalueResult> key_lv = LowerLvalue(data.args[0], builder);
      if (!key_lv) return std::unexpected(key_lv.error());
      mir::PlaceId dest = ctx.AllocTemp(expr.type);
      if (data.method == hir::BuiltinMethod::kAssocNext) {
        builder.EmitAssocOp(
            mir::AssocOp{
                .receiver = lv_result->GetLocalPlace(),
                .data = mir::AssocIterNext{
                    .dest_found = dest, .key_place = key_lv->GetLocalPlace()}});
      } else {
        builder.EmitAssocOp(
            mir::AssocOp{
                .receiver = lv_result->GetLocalPlace(),
                .data = mir::AssocIterPrev{
                    .dest_found = dest, .key_place = key_lv->GetLocalPlace()}});
      }
      return mir::Operand::Use(dest);
    }

    case hir::BuiltinMethod::kAssocSnapshot: {
      Result<LvalueResult> lv_result = LowerLvalue(data.receiver, builder);
      if (!lv_result) return std::unexpected(lv_result.error());
      mir::PlaceId dest = ctx.AllocTemp(expr.type);
      builder.EmitAssocOp(
          mir::AssocOp{
              .receiver = lv_result->GetLocalPlace(),
              .data = mir::AssocSnapshot{.dest_keys = dest}});
      return mir::Operand::Use(dest);
    }

    case hir::BuiltinMethod::kEnumNext:
    case hir::BuiltinMethod::kEnumPrev: {
      // operands[0] = receiver value
      Result<mir::Operand> receiver_result =
          LowerExpressionImpl(data.receiver, builder, cache);
      if (!receiver_result) return std::unexpected(receiver_result.error());
      mir::Operand receiver_val = *receiver_result;
      std::vector<mir::Operand> operands = {receiver_val};

      // operands[1] = optional step N
      if (!data.args.empty()) {
        Result<mir::Operand> step_result =
            LowerExpressionImpl(data.args[0], builder, cache);
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

      mir::PlaceId temp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }

    case hir::BuiltinMethod::kEnumName: {
      // operands[0] = receiver value
      Result<mir::Operand> receiver_result =
          LowerExpressionImpl(data.receiver, builder, cache);
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

      mir::PlaceId temp = builder.EmitPlaceTemp(expr.type, std::move(rvalue));
      return mir::Operand::Use(temp);
    }
  }
  throw common::InternalError(
      "LowerBuiltinMethodCall", "unknown builtin method");
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

  auto valid = builder.EmitIndexAccessValidity(
      index, index_type, range.Lower(), range.Upper());

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

  auto valid = builder.EmitIndexAccessValidity(index, index_type, lower, upper);

  // Offset = index (0-based descending)
  return {index, valid};
}

auto LowerPackedElementSelect(
    const hir::PackedElementSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  Result<mir::Operand> base_result =
      LowerExpressionImpl(data.base, builder, cache);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  BlockIndex before_index = builder.CurrentBlock();
  Result<mir::Operand> index_result =
      LowerExpressionImpl(data.index, builder, cache);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;
  FixupOperandsAfterBlockChange(builder, before_index, {&base_operand});

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];
  uint32_t element_width = PackedArrayElementWidth(base_type, *ctx.type_arena);

  // Compute offset and validity
  auto [offset, valid] = EmitPackedElementOffset(
      index_operand, index_expr.type, base_type, builder);

  // Get base as Place (memoize UseTemp -> PlaceTemp materialization)
  mir::PlaceId base_place =
      builder.EnsurePlaceCached(base_expr.type, base_operand, cache);

  // Create BitRangeProjection (address-only)
  mir::PlaceId slice_place = ctx.DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = element_width,
                          .element_type = expr.type,
                          .guaranteed_alignment_bits =
                              PowerOfTwoAlignment(element_width)}});

  // Use GuardedUse for OOB-safe read: valid ? Use(place) : oob_default
  return builder.EmitGuardedUse(valid, slice_place, expr.type);
}

auto LowerBitSelect(
    const hir::BitSelectExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  Result<mir::Operand> base_result =
      LowerExpressionImpl(data.base, builder, cache);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  BlockIndex before_index = builder.CurrentBlock();
  Result<mir::Operand> index_result =
      LowerExpressionImpl(data.index, builder, cache);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;
  FixupOperandsAfterBlockChange(builder, before_index, {&base_operand});

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute offset and validity
  auto [offset, valid] =
      EmitBitSelectOffset(index_operand, index_expr.type, base_type, builder);

  // Get base as Place (memoize UseTemp -> PlaceTemp materialization)
  mir::PlaceId base_place =
      builder.EnsurePlaceCached(base_expr.type, base_operand, cache);

  // Create BitRangeProjection (width=1)
  mir::PlaceId slice_place = ctx.DerivePlace(
      base_place,
      mir::Projection{
          .info = mir::BitRangeProjection{
              .bit_offset = offset, .width = 1, .element_type = expr.type}});

  // Use GuardedUse for OOB-safe read: valid ? Use(place) : oob_default
  return builder.EmitGuardedUse(valid, slice_place, expr.type);
}

auto LowerRangeSelect(
    const hir::RangeSelectExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base expression
  Result<mir::Operand> base_result =
      LowerExpressionImpl(data.base, builder, cache);
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

  // Get base as Place (memoize UseTemp -> PlaceTemp materialization)
  mir::PlaceId base_place =
      builder.EnsurePlaceCached(base_expr.type, base_operand, cache);

  if (bit_offset < 0 || static_cast<uint32_t>(bit_offset) + width >
                            PackedBitWidth(base_type, *ctx.type_arena)) {
    throw common::InternalError(
        "LowerRangeSelect", "bit_offset + width exceeds base bit width");
  }

  // Create BitRangeProjection (address-only)
  mir::PlaceId slice_place = ctx.DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = width,
                          .element_type = expr.type}});

  // Range select with constant bounds is always valid - no Select wrapper
  // needed
  return mir::Operand::Use(slice_place);
}

// Compute offset, validity, and alignment for indexed part-select.
// For ascending (+:): select bits [index .. index + width - 1]
// For descending (-:): select bits [index - width + 1 .. index]
// Returns {offset, valid, alignment} tuple.
auto EmitIndexedPartSelectOffset(
    mir::Operand index, TypeId index_type, const Type& base_type,
    uint32_t width, bool ascending, uint32_t index_alignment,
    MirBuilder& builder) -> std::tuple<mir::Operand, mir::Operand, uint32_t> {
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
    valid = builder.EmitIndexAccessValidity(
        index, index_type, static_cast<int32_t>(eff_lower_64),
        static_cast<int32_t>(eff_upper_64));
  }

  // Compute physical bit offset and alignment. Alignment is co-produced
  // with the offset so there is one authority for both.
  mir::Operand offset;
  int64_t subtracted_constant = 0;
  if (base_descending) {
    if (ascending) {
      subtracted_constant = lower;
      auto lower_const =
          mir::Operand::Const(MakeIntegralConst(lower, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, lower_const, offset_type);
    } else {
      subtracted_constant = static_cast<int64_t>(lower) + width - 1;
      auto adjust_const = mir::Operand::Const(
          MakeIntegralConst(subtracted_constant, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, adjust_const, offset_type);
    }
  } else {
    if (ascending) {
      subtracted_constant = static_cast<int64_t>(upper) - width + 1;
      auto adjust_const = mir::Operand::Const(
          MakeIntegralConst(subtracted_constant, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, adjust_const, index, offset_type);
    } else {
      subtracted_constant = upper;
      auto upper_const =
          mir::Operand::Const(MakeIntegralConst(upper, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, upper_const, index, offset_type);
    }
  }

  uint32_t alignment =
      ComputePartSelectOffsetAlignment(index_alignment, subtracted_constant);

  return {offset, valid, alignment};
}

auto LowerIndexedPartSelect(
    const hir::IndexedPartSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base and index expressions
  Result<mir::Operand> base_result =
      LowerExpressionImpl(data.base, builder, cache);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  BlockIndex before_index = builder.CurrentBlock();
  Result<mir::Operand> index_result =
      LowerExpressionImpl(data.index, builder, cache);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;
  FixupOperandsAfterBlockChange(builder, before_index, {&base_operand});

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute index alignment from HIR expression tree.
  uint32_t index_alignment = GetHirExpressionAlignmentBits(
      *ctx.hir_arena, *ctx.active_constant_arena, data.index);

  // Compute offset, validity, and alignment together.
  auto [offset, valid, alignment] = EmitIndexedPartSelectOffset(
      index_operand, index_expr.type, base_type, data.width, data.ascending,
      index_alignment, builder);

  // Get base as Place (memoize UseTemp -> PlaceTemp materialization)
  mir::PlaceId base_place =
      builder.EnsurePlaceCached(base_expr.type, base_operand, cache);

  // Create BitRangeProjection
  mir::PlaceId slice_place = ctx.DerivePlace(
      base_place, mir::Projection{
                      .info = mir::BitRangeProjection{
                          .bit_offset = offset,
                          .width = data.width,
                          .element_type = expr.type,
                          .guaranteed_alignment_bits = alignment}});

  // Use GuardedUse for OOB-safe read: valid ? Use(place) : oob_default
  return builder.EmitGuardedUse(valid, slice_place, expr.type);
}

auto LowerPackedFieldAccess(
    const hir::PackedFieldAccessExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Lower base expression
  Result<mir::Operand> base_result =
      LowerExpressionImpl(data.base, builder, cache);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  // Get base expression type
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];

  // Create constant offset operand
  mir::Operand offset = mir::Operand::Const(
      MakeIntegralConst(data.bit_offset, ctx.GetOffsetType()));

  // Get base as Place (memoize UseTemp -> PlaceTemp materialization)
  mir::PlaceId base_place =
      builder.EnsurePlaceCached(base_expr.type, base_operand, cache);

  const Type& base_type = (*ctx.type_arena)[base_expr.type];
  if (data.bit_offset + data.bit_width >
      PackedBitWidth(base_type, *ctx.type_arena)) {
    throw common::InternalError(
        "LowerPackedFieldAccess",
        "bit_offset + bit_width exceeds base bit width");
  }

  // Create BitRangeProjection (address-only)
  mir::PlaceId slice_place = ctx.DerivePlace(
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
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  const Context& ctx = builder.GetContext();
  const auto& types = *ctx.type_arena;

  // Single-operand concat with packed result and non-packed operand is actually
  // a type conversion (e.g., string -> packed). Slang uses concat for implicit
  // conversions, but backends expect packed concat operands to be packed.
  // Emit Cast to make the conversion explicit. We use Cast (not BitCast)
  // because string<->packed is a semantic conversion with padding/truncation
  // rules, not a bit-level reinterpretation.
  if (data.operands.size() == 1) {
    const hir::Expression& operand_expr = (*ctx.hir_arena)[data.operands[0]];
    bool result_is_packed = IsPacked(types[expr.type]);
    bool operand_is_packed = IsPacked(types[operand_expr.type]);

    if (result_is_packed && !operand_is_packed) {
      Result<mir::Operand> op_result =
          LowerExpressionImpl(data.operands[0], builder, cache);
      if (!op_result) return std::unexpected(op_result.error());

      mir::Rvalue rvalue{
          .operands = {*op_result},
          .info =
              mir::CastRvalueInfo{
                  .source_type = operand_expr.type, .target_type = expr.type},
      };
      return builder.EmitValueTemp(expr.type, std::move(rvalue));
    }
  }

  std::vector<mir::Operand> operands;
  operands.reserve(data.operands.size());
  for (hir::ExpressionId op_id : data.operands) {
    BlockIndex before = builder.CurrentBlock();
    Result<mir::Operand> op_result = LowerExpressionImpl(op_id, builder, cache);
    if (!op_result) return std::unexpected(op_result.error());
    if (builder.CurrentBlock() != before) {
      for (auto& op : operands) {
        op = builder.ThreadValueToCurrentBlock(op);
      }
    }
    operands.push_back(*op_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::ConcatRvalueInfo{.result_type = expr.type},
  };
  return builder.EmitValueTemp(expr.type, std::move(rvalue));
}

auto LowerReplicate(
    const hir::ReplicateExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Result<mir::Operand> elem_result =
      LowerExpressionImpl(data.element, builder, cache);
  if (!elem_result) return std::unexpected(elem_result.error());

  mir::Rvalue rvalue{
      .operands = {*elem_result},
      .info = mir::ReplicateRvalueInfo{
          .result_type = expr.type, .count = data.count}};
  return builder.EmitValueTemp(expr.type, std::move(rvalue));
}

auto LowerMathCall(
    const hir::MathCallExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
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
    BlockIndex before = builder.CurrentBlock();
    Result<mir::Operand> arg_result =
        LowerExpressionImpl(arg_id, builder, cache);
    if (!arg_result) return std::unexpected(arg_result.error());
    if (builder.CurrentBlock() != before) {
      for (auto& op : operands) {
        op = builder.ThreadValueToCurrentBlock(op);
      }
    }
    operands.push_back(*arg_result);
  }

  mir::Rvalue rvalue{
      .operands = std::move(operands),
      .info = mir::MathCallRvalueInfo{.fn = data.fn},
  };
  return builder.EmitValueTemp(expr.type, std::move(rvalue));
}

auto LowerElementAccessRvalue(
    const hir::ElementAccessExpressionData& data, const hir::Expression& expr,
    MirBuilder& builder, PlaceMaterializationCache& cache)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  Result<mir::Operand> base_result =
      LowerExpressionImpl(data.base, builder, cache);
  if (!base_result) return std::unexpected(base_result.error());
  mir::Operand base_operand = *base_result;

  BlockIndex before_index = builder.CurrentBlock();
  Result<mir::Operand> index_result =
      LowerExpressionImpl(data.index, builder, cache);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = *index_result;
  FixupOperandsAfterBlockChange(builder, before_index, {&base_operand});

  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  // Use EnsurePlaceCached for memoization (6th projection-base site)
  mir::PlaceId base_place =
      builder.EnsurePlaceCached(base_expr.type, base_operand, cache);

  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Associative array: emit AssocGet
  if (base_type.Kind() == TypeKind::kAssociativeArray) {
    const auto& aa_info = base_type.AsAssociativeArray();
    mir::PlaceId dest = ctx.AllocTemp(aa_info.element_type);
    builder.EmitAssocOp(
        mir::AssocOp{
            .receiver = base_place,
            .data = mir::AssocGet{.dest = dest, .key = index_operand}});
    return mir::Operand::Use(dest);
  }

  if (base_type.Kind() != TypeKind::kUnpackedArray &&
      base_type.Kind() != TypeKind::kDynamicArray &&
      base_type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "LowerElementAccessRvalue", "base is not an array or queue");
  }

  // Compute validity against original index (before normalization).
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  mir::Operand validity = EmitUnpackedIndexValidity(
      index_operand, index_expr.type, base_place, base_expr.type, builder);

  // Normalize index for addressing (after validity).
  index_operand = NormalizeUnpackedIndex(
      index_operand, index_expr.type, base_type, builder);

  mir::Projection proj{
      .info = mir::IndexProjection{.index = index_operand},
  };
  mir::PlaceId result_place = ctx.DerivePlace(base_place, std::move(proj));

  if (IsAlwaysValid(validity)) {
    return mir::Operand::Use(result_place);
  }

  // GuardedUse only works for packed types (LLVM backend uses integer ops).
  // For non-packed types (e.g., dynamic array handle in multi-dim access),
  // use a branch to guard the read and provide a default-initialized temp.
  const Type& element_type = (*ctx.type_arena)[expr.type];
  if (IsPacked(element_type)) {
    return builder.EmitGuardedUse(validity, result_place, expr.type);
  }

  // Branch guard for non-packed element reads.
  // OOB default: zero for 2-state / handles, X for 4-state sub-content.
  mir::PlaceId result_temp = ctx.AllocTemp(expr.type);
  mir::Operand cond = builder.MaterializeForBranch(validity);
  BlockIndex read_bb = builder.CreateBlock();
  BlockIndex oob_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();
  builder.EmitBranch(cond, read_bb, oob_bb);

  builder.SetCurrentBlock(read_bb);
  builder.EmitAssign(result_temp, mir::Operand::Use(result_place));
  builder.EmitJump(merge_bb);

  // OOB path: fill 4-state sub-content with X (2-state stays zero).
  builder.SetCurrentBlock(oob_bb);
  if (IsIntrinsicallyFourState(expr.type, *ctx.type_arena)) {
    EmitOOBDefault(result_temp, expr.type, builder);
  }
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
  return mir::Operand::Use(result_temp);
}

auto LowerMaterializeInitializer(
    const hir::MaterializeInitializerExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();

  // Create a temp to hold the materialized initializer value
  mir::PlaceId temp = ctx.AllocTemp(expr.type);

  // Use LowerPattern to emit the fill effect into the temp
  auto result = LowerPattern(data.pattern, temp, builder);
  if (!result) {
    return std::unexpected(result.error());
  }

  // Return use of the temp as the expression value
  return mir::Operand::Use(temp);
}

// Main internal lowering implementation with cache.
// All recursive calls must go through this function (not the public wrapper).
auto LowerExpressionImpl(
    hir::ExpressionId expr_id, MirBuilder& builder,
    PlaceMaterializationCache& cache) -> Result<mir::Operand> {
  const hir::Expression& expr = (*builder.GetContext().hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> Result<mir::Operand> {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::ConstantExpressionData>) {
          return LowerConstant(data, builder);
        } else if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return LowerNameRef(data, builder);
        } else if constexpr (std::is_same_v<T, hir::UnaryExpressionData>) {
          return LowerUnary(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::BinaryExpressionData>) {
          return LowerBinary(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::CastExpressionData>) {
          return LowerCast(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::BitCastExpressionData>) {
          return LowerBitCast(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::SystemCallExpressionData>) {
          return LowerSystemCall(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::ConditionalExpressionData>) {
          return LowerConditional(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::AssignmentExpressionData>) {
          return LowerAssignment(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::CompoundAssignmentExpressionData>) {
          return LowerCompoundAssignment(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          return LowerElementAccessRvalue(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          Context& ctx = builder.GetContext();
          const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
          if (const auto* lit = std::get_if<hir::StructLiteralExpressionData>(
                  &base_expr.data)) {
            // Struct literal field access - extract directly without lvalue.
            // field_index and field_values are both in declaration order.
            if (data.field_index < 0 || static_cast<size_t>(data.field_index) >=
                                            lit->field_values.size()) {
              throw common::InternalError(
                  "MemberAccessExpression",
                  "field_index out of range for struct literal");
            }
            return LowerExpressionImpl(
                lit->field_values[data.field_index], builder, cache);
          }
          // Addressable base - use existing lvalue+load path (separate scope)
          Result<LvalueResult> lv_result = LowerLvalue(expr_id, builder);
          if (!lv_result) return std::unexpected(lv_result.error());
          LvalueResult lv = std::move(*lv_result);
          return mir::Operand::Use(lv.GetLocalPlace());
        } else if constexpr (std::is_same_v<
                                 T, hir::UnionMemberAccessExpressionData>) {
          // Union member access - always valid (member index is compile-time)
          // Note: LowerLvalue has separate cache scope
          Result<LvalueResult> lv_result = LowerLvalue(expr_id, builder);
          if (!lv_result) return std::unexpected(lv_result.error());
          LvalueResult lv = std::move(*lv_result);
          return mir::Operand::Use(lv.GetLocalPlace());
        } else if constexpr (std::is_same_v<
                                 T, hir::StructLiteralExpressionData>) {
          return LowerStructLiteral(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::ArrayLiteralExpressionData>) {
          return LowerArrayLiteral(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::CallExpressionData>) {
          return LowerCall(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::NewArrayExpressionData>) {
          return LowerNewArray(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::BuiltinMethodCallExpressionData>) {
          return LowerBuiltinMethodCall(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedElementSelectExpressionData>) {
          return LowerPackedElementSelect(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::BitSelectExpressionData>) {
          return LowerBitSelect(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::RangeSelectExpressionData>) {
          return LowerRangeSelect(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::IndexedPartSelectExpressionData>) {
          return LowerIndexedPartSelect(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedFieldAccessExpressionData>) {
          return LowerPackedFieldAccess(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::ConcatExpressionData>) {
          return LowerConcat(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<T, hir::ReplicateExpressionData>) {
          return LowerReplicate(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T, hir::HierarchicalRefExpressionData>) {
          auto& ctx = builder.GetContext();
          if (ctx.external_refs == nullptr) {
            throw common::InternalError(
                "LowerExpression/HierarchicalRef",
                "hierarchical body read requires external-ref context");
          }
          auto ref = ctx.LowerHierarchicalRefToExternalRef(
              data, expr.type, mir::ExternalAccessKind::kRead);
          return builder.EmitExternalRead(expr.type, ref.ref_id);
        } else if constexpr (std::is_same_v<T, hir::MathCallExpressionData>) {
          return LowerMathCall(data, expr, builder, cache);
        } else if constexpr (std::is_same_v<
                                 T,
                                 hir::MaterializeInitializerExpressionData>) {
          return LowerMaterializeInitializer(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerExpression", "unhandled expression kind");
        }
      },
      expr.data);
}

}  // namespace

// Public entry point: creates fresh cache and delegates to implementation.
auto LowerExpression(hir::ExpressionId expr_id, MirBuilder& builder)
    -> Result<mir::Operand> {
  PlaceMaterializationCache cache;
  return LowerExpressionImpl(expr_id, builder, cache);
}

}  // namespace lyra::lowering::hir_to_mir
