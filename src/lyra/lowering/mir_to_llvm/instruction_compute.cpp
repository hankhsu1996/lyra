#include "lyra/lowering/mir_to_llvm/instruction_compute.hpp"

#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/ADT/APInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ErrorHandling.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/lowering/mir_to_llvm/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Type classification for LLVM lowering
enum class PlaceKind {
  kIntegral,  // Packed integral types (bit, logic, int, etc.)
  kString,    // String type
};

// Type info for a place, used to select the lowering strategy
struct PlaceTypeInfo {
  PlaceKind kind;
  uint32_t bit_width;  // Only valid for kIntegral
  bool is_four_state;  // Only valid for kIntegral
};

// Check if a packed type contains a packed struct (at any level)
auto ContainsPackedStruct(const Type& type, const TypeArena& types) -> bool {
  if (type.Kind() == TypeKind::kPackedStruct) {
    return true;
  }
  if (type.Kind() == TypeKind::kPackedArray) {
    return ContainsPackedStruct(
        types[type.AsPackedArray().element_type], types);
  }
  return false;
}

// Validates type is supported and returns PlaceTypeInfo
auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> PlaceTypeInfo {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[place_id];
  const Type& type = types[place.root.type];

  // Handle string type
  if (type.Kind() == TypeKind::kString) {
    return PlaceTypeInfo{
        .kind = PlaceKind::kString,
        .bit_width = 0,
        .is_four_state = false,
    };
  }

  // Handle packed integral types
  if (!IsPacked(type)) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
        context.GetCurrentOrigin(),
        std::format("non-packed type not supported: {}", ToString(type)));
  }
  if (ContainsPackedStruct(type, types)) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(), "packed structs not yet supported");
  }
  return PlaceTypeInfo{
      .kind = PlaceKind::kIntegral,
      .bit_width = PackedBitWidth(type, types),
      .is_four_state = IsPackedFourState(type, types),
  };
}

// Get mask with semantic_width low bits set (for width masking and all-ones)
auto GetSemanticMask(llvm::Type* ty, uint32_t semantic_width) -> llvm::Value* {
  auto mask =
      llvm::APInt::getLowBitsSet(ty->getIntegerBitWidth(), semantic_width);
  return llvm::ConstantInt::get(ty, mask);
}

// Masks result to semantic width if needed
auto ApplyWidthMask(
    Context& context, llvm::Value* value, uint32_t semantic_width)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  uint32_t storage_width = value->getType()->getIntegerBitWidth();

  if (semantic_width == 0) {
    throw common::InternalError("ApplyWidthMask", "semantic width cannot be 0");
  }
  if (semantic_width > storage_width) {
    throw common::InternalError(
        "ApplyWidthMask", std::format(
                              "semantic width ({}) exceeds storage width ({})",
                              semantic_width, storage_width));
  }

  // No masking needed if semantic == storage
  if (semantic_width == storage_width) {
    return value;
  }

  return builder.CreateAnd(
      value, GetSemanticMask(value->getType(), semantic_width), "mask");
}

// Dispatch to LLVM add/sub/mul
auto LowerBinaryArith(
    Context& context, mir::BinaryOp op, llvm::Value* lhs, llvm::Value* rhs)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  switch (op) {
    case mir::BinaryOp::kAdd:
      return builder.CreateAdd(lhs, rhs, "add");
    case mir::BinaryOp::kSubtract:
      return builder.CreateSub(lhs, rhs, "sub");
    case mir::BinaryOp::kMultiply:
      return builder.CreateMul(lhs, rhs, "mul");
    case mir::BinaryOp::kBitwiseAnd:
      return builder.CreateAnd(lhs, rhs, "and");
    case mir::BinaryOp::kBitwiseOr:
      return builder.CreateOr(lhs, rhs, "or");
    case mir::BinaryOp::kBitwiseXor:
      return builder.CreateXor(lhs, rhs, "xor");
    case mir::BinaryOp::kBitwiseXnor: {
      auto* xor_result = builder.CreateXor(lhs, rhs, "xor");
      return builder.CreateNot(xor_result, "xnor");
    }

    // Logical operators - convert to bool first, then AND/OR
    case mir::BinaryOp::kLogicalAnd: {
      auto* const_zero = llvm::ConstantInt::get(lhs->getType(), 0);
      auto* lhs_bool = builder.CreateICmpNE(lhs, const_zero, "lhs.bool");
      // NOLINTNEXTLINE(readability-suspicious-call-argument)
      auto* rhs_bool = builder.CreateICmpNE(rhs, const_zero, "rhs.bool");
      return builder.CreateAnd(lhs_bool, rhs_bool, "land");
    }
    case mir::BinaryOp::kLogicalOr: {
      auto* const_zero = llvm::ConstantInt::get(lhs->getType(), 0);
      auto* lhs_bool = builder.CreateICmpNE(lhs, const_zero, "lhs.bool");
      // NOLINTNEXTLINE(readability-suspicious-call-argument)
      auto* rhs_bool = builder.CreateICmpNE(rhs, const_zero, "rhs.bool");
      return builder.CreateOr(lhs_bool, rhs_bool, "lor");
    }

    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format("unsupported binary op: {}", mir::ToString(op)));
  }
}

// Check if the binary op returns i1 (comparisons and logical ops)
auto ReturnsI1(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kEqual:
    case mir::BinaryOp::kNotEqual:
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kLessThanSigned:
    case mir::BinaryOp::kLessThanEqualSigned:
    case mir::BinaryOp::kGreaterThanSigned:
    case mir::BinaryOp::kGreaterThanEqualSigned:
    case mir::BinaryOp::kLogicalAnd:
    case mir::BinaryOp::kLogicalOr:
      return true;
    default:
      return false;
  }
}

// Check if the binary op is a comparison (needs icmp)
auto IsComparisonOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kEqual:
    case mir::BinaryOp::kNotEqual:
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kLessThanSigned:
    case mir::BinaryOp::kLessThanEqualSigned:
    case mir::BinaryOp::kGreaterThanSigned:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      return true;
    default:
      return false;
  }
}

// Check if the binary op is a shift (needs overflow guards)
auto IsShiftOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kLogicalShiftLeft:
    case mir::BinaryOp::kArithmeticShiftLeft:
    case mir::BinaryOp::kLogicalShiftRight:
    case mir::BinaryOp::kArithmeticShiftRight:
      return true;
    default:
      return false;
  }
}

// Check if the unary op is a reduction (needs original operand type)
auto IsReductionOp(mir::UnaryOp op) -> bool {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
    case mir::UnaryOp::kReductionNand:
    case mir::UnaryOp::kReductionOr:
    case mir::UnaryOp::kReductionNor:
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionXnor:
      return true;
    default:
      return false;
  }
}

// Lower shift operation with overflow guards
// LLVM shifts produce poison when shift_amount >= bitwidth, so we guard:
// - shl/lshr overflow -> 0
// - ashr overflow -> sign-fill (all 1s if negative, all 0s if positive)
auto LowerShiftOp(
    Context& context, mir::BinaryOp op, llvm::Value* value,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  llvm::Type* ty = value->getType();

  auto* width_const = llvm::ConstantInt::get(ty, semantic_width);
  auto* zero = llvm::ConstantInt::get(ty, 0);

  // Guard: in_range = (shift_amount < semantic_width)
  auto* in_range =
      builder.CreateICmpULT(shift_amount, width_const, "shift.inrange");

  // Safe shift amount (use 0 if out of range to avoid poison)
  auto* safe_amount =
      builder.CreateSelect(in_range, shift_amount, zero, "shift.safe");

  llvm::Value* shifted = nullptr;
  llvm::Value* fallback = zero;

  switch (op) {
    case mir::BinaryOp::kLogicalShiftLeft:
    case mir::BinaryOp::kArithmeticShiftLeft:
      shifted = builder.CreateShl(value, safe_amount, "shl");
      break;
    case mir::BinaryOp::kLogicalShiftRight:
      shifted = builder.CreateLShr(value, safe_amount, "lshr");
      break;
    case mir::BinaryOp::kArithmeticShiftRight: {
      shifted = builder.CreateAShr(value, safe_amount, "ashr");
      // Sign-fill fallback: all 1s in semantic bits if sign bit set, else 0
      auto* width_m1 = llvm::ConstantInt::get(ty, semantic_width - 1);
      auto* sign_bit = builder.CreateLShr(value, width_m1, "signbit");
      auto* is_neg = builder.CreateTrunc(sign_bit, builder.getInt1Ty());
      auto* semantic_ones = GetSemanticMask(ty, semantic_width);
      fallback = builder.CreateSelect(is_neg, semantic_ones, zero, "signfill");
      break;
    }
    default:
      llvm_unreachable("not a shift op");
  }

  return builder.CreateSelect(in_range, shifted, fallback, "shift.result");
}

// Dispatch to LLVM icmp for comparison operators
// Returns i1, caller must extend to storage type
auto LowerBinaryComparison(
    Context& context, mir::BinaryOp op, llvm::Value* lhs, llvm::Value* rhs)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  switch (op) {
    case mir::BinaryOp::kEqual:
      return builder.CreateICmpEQ(lhs, rhs, "eq");
    case mir::BinaryOp::kNotEqual:
      return builder.CreateICmpNE(lhs, rhs, "ne");
    case mir::BinaryOp::kLessThan:
      return builder.CreateICmpULT(lhs, rhs, "ult");
    case mir::BinaryOp::kLessThanEqual:
      return builder.CreateICmpULE(lhs, rhs, "ule");
    case mir::BinaryOp::kGreaterThan:
      return builder.CreateICmpUGT(lhs, rhs, "ugt");
    case mir::BinaryOp::kGreaterThanEqual:
      return builder.CreateICmpUGE(lhs, rhs, "uge");
    case mir::BinaryOp::kLessThanSigned:
      return builder.CreateICmpSLT(lhs, rhs, "slt");
    case mir::BinaryOp::kLessThanEqualSigned:
      return builder.CreateICmpSLE(lhs, rhs, "sle");
    case mir::BinaryOp::kGreaterThanSigned:
      return builder.CreateICmpSGT(lhs, rhs, "sgt");
    case mir::BinaryOp::kGreaterThanEqualSigned:
      return builder.CreateICmpSGE(lhs, rhs, "sge");
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format("unsupported comparison op: {}", mir::ToString(op)));
  }
}

// Check if an operand has string type
auto IsStringOperand(Context& context, const mir::Operand& operand) -> bool {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      Overloaded{
          [&](const Constant& c) {
            return types[c.type].Kind() == TypeKind::kString;
          },
          [&](mir::PlaceId place_id) {
            const auto& place = arena[place_id];
            return types[place.root.type].Kind() == TypeKind::kString;
          },
      },
      operand.payload);
}

// Get the semantic bit width of an operand (for reduction operators)
auto GetOperandBitWidth(Context& context, const mir::Operand& operand)
    -> uint32_t {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      Overloaded{
          [&](const Constant& c) {
            return PackedBitWidth(types[c.type], types);
          },
          [&](mir::PlaceId place_id) {
            const auto& place = arena[place_id];
            return PackedBitWidth(types[place.root.type], types);
          },
      },
      operand.payload);
}

// Lower string binary comparison via LyraStringCmp runtime call
auto LowerStringBinaryOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* result_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  // Only comparison operators are supported for strings
  if (!IsComparisonOp(info.op)) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm,
        common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
        std::format(
            "string operation not supported (only comparisons): {}",
            mir::ToString(info.op)));
  }

  // Lower both operands (they return string handles from LyraStringFromLiteral)
  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

  // Register owned temps (literals) for cleanup at statement end
  // Constants get a freshly allocated string from LyraStringFromLiteral,
  // which we own and must release. Place references are borrowed.
  if (std::holds_alternative<Constant>(operands[0].payload)) {
    context.RegisterOwnedTemp(lhs);
  }
  if (std::holds_alternative<Constant>(operands[1].payload)) {
    context.RegisterOwnedTemp(rhs);
  }

  // Call LyraStringCmp(lhs, rhs) -> i32
  llvm::Value* cmp_result =
      builder.CreateCall(context.GetLyraStringCmp(), {lhs, rhs}, "strcmp");

  // Convert cmp result to boolean based on operator
  auto* zero = llvm::ConstantInt::get(cmp_result->getType(), 0);
  llvm::Value* bool_result = nullptr;

  switch (info.op) {
    case mir::BinaryOp::kEqual:
      bool_result = builder.CreateICmpEQ(cmp_result, zero, "str.eq");
      break;
    case mir::BinaryOp::kNotEqual:
      bool_result = builder.CreateICmpNE(cmp_result, zero, "str.ne");
      break;
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanSigned:
      bool_result = builder.CreateICmpSLT(cmp_result, zero, "str.lt");
      break;
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kLessThanEqualSigned:
      bool_result = builder.CreateICmpSLE(cmp_result, zero, "str.le");
      break;
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanSigned:
      bool_result = builder.CreateICmpSGT(cmp_result, zero, "str.gt");
      break;
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      bool_result = builder.CreateICmpSGE(cmp_result, zero, "str.ge");
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format(
              "unsupported string comparison: {}", mir::ToString(info.op)));
  }

  // Extend to result type
  return builder.CreateZExt(bool_result, result_type, "str.cmp.ext");
}

auto LowerBinaryRvalue(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type,
    uint32_t semantic_width) -> llvm::Value* {
  // Dispatch to string lowering if operands are strings
  if (IsStringOperand(context, operands[0])) {
    return LowerStringBinaryOp(context, info, operands, storage_type);
  }

  auto& builder = context.GetBuilder();

  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

  // Coerce both operands to storage type BEFORE operation
  lhs = builder.CreateZExtOrTrunc(lhs, storage_type, "lhs.coerce");
  rhs = builder.CreateZExtOrTrunc(rhs, storage_type, "rhs.coerce");

  // Comparison operators use icmp, which returns i1
  if (IsComparisonOp(info.op)) {
    llvm::Value* cmp = LowerBinaryComparison(context, info.op, lhs, rhs);
    return builder.CreateZExt(cmp, storage_type, "cmp.ext");
  }

  // Shift operators need overflow guards
  if (IsShiftOp(info.op)) {
    return LowerShiftOp(context, info.op, lhs, rhs, semantic_width);
  }

  // Arithmetic/bitwise/logical operators
  llvm::Value* result = LowerBinaryArith(context, info.op, lhs, rhs);

  // Logical operators return i1, need to extend to storage type
  if (ReturnsI1(info.op)) {
    return builder.CreateZExt(result, storage_type, "bool.ext");
  }

  return result;
}

// Dispatch to LLVM instructions for unary operators
// operand_bit_width is the semantic width of the operand (needed for reduction)
auto LowerUnaryOp(
    Context& context, mir::UnaryOp op, llvm::Value* operand,
    llvm::Type* storage_type, uint32_t operand_bit_width) -> llvm::Value* {
  auto& builder = context.GetBuilder();

  switch (op) {
    case mir::UnaryOp::kPlus:
      return operand;
    case mir::UnaryOp::kMinus:
      return builder.CreateNeg(operand, "neg");
    case mir::UnaryOp::kBitwiseNot:
      return builder.CreateNot(operand, "not");
    case mir::UnaryOp::kLogicalNot: {
      // Convert to bool (compare != 0), then negate
      auto* zero = llvm::ConstantInt::get(operand->getType(), 0);
      auto* is_nonzero = builder.CreateICmpNE(operand, zero, "nonzero");
      auto* negated = builder.CreateNot(is_nonzero, "lnot");
      return builder.CreateZExt(negated, storage_type, "lnot.ext");
    }
    case mir::UnaryOp::kReductionAnd: {
      // Returns 1 if all bits are 1, else 0
      auto* all_ones = GetSemanticMask(operand->getType(), operand_bit_width);
      auto* is_all_ones = builder.CreateICmpEQ(operand, all_ones, "red.and");
      return builder.CreateZExt(is_all_ones, storage_type, "red.and.ext");
    }
    case mir::UnaryOp::kReductionNand: {
      // Returns 0 if all bits are 1, else 1 (opposite of AND)
      auto* all_ones = GetSemanticMask(operand->getType(), operand_bit_width);
      auto* is_not_all_ones =
          builder.CreateICmpNE(operand, all_ones, "red.nand");
      return builder.CreateZExt(is_not_all_ones, storage_type, "red.nand.ext");
    }
    case mir::UnaryOp::kReductionOr: {
      // Returns 1 if any bit is 1, else 0
      auto* zero = llvm::ConstantInt::get(operand->getType(), 0);
      auto* is_nonzero = builder.CreateICmpNE(operand, zero, "red.or");
      return builder.CreateZExt(is_nonzero, storage_type, "red.or.ext");
    }
    case mir::UnaryOp::kReductionNor: {
      // Returns 1 if all bits are 0, else 0 (opposite of OR)
      auto* zero = llvm::ConstantInt::get(operand->getType(), 0);
      auto* is_zero = builder.CreateICmpEQ(operand, zero, "red.nor");
      return builder.CreateZExt(is_zero, storage_type, "red.nor.ext");
    }
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionXnor: {
      // Parity: 1 if odd number of bits are 1, 0 if even
      // Use llvm.ctpop to count bits, then extract LSB
      auto* ctpop = llvm::Intrinsic::getDeclaration(
          &context.GetModule(), llvm::Intrinsic::ctpop, {operand->getType()});
      auto* count = builder.CreateCall(ctpop, {operand}, "popcount");
      auto* one = llvm::ConstantInt::get(count->getType(), 1);
      auto* parity_n = builder.CreateAnd(count, one, "parity");
      auto* parity_i1 =
          builder.CreateTrunc(parity_n, builder.getInt1Ty(), "parity.i1");

      llvm::Value* result = parity_i1;
      if (op == mir::UnaryOp::kReductionXnor) {
        // XNOR is inverted XOR
        result = builder.CreateXor(result, builder.getTrue(), "parity.not");
      }

      const char* name =
          (op == mir::UnaryOp::kReductionXnor) ? "red.xnor.ext" : "red.xor.ext";
      return builder.CreateZExt(result, storage_type, name);
    }
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format("unsupported unary op: {}", mir::ToString(op)));
  }
}

// Lower unary rvalue
auto LowerUnaryRvalue(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  // Get operand's semantic bit width (needed for reduction operators)
  uint32_t operand_bit_width = GetOperandBitWidth(context, operands[0]);

  // Lower the operand
  llvm::Value* operand = LowerOperand(context, operands[0]);

  // Reduction operators work on the source type and return a boolean.
  // Don't coerce them - they need the full source value to compare against.
  if (!IsReductionOp(info.op)) {
    operand = builder.CreateZExtOrTrunc(operand, storage_type, "op.coerce");
  }

  return LowerUnaryOp(
      context, info.op, operand, storage_type, operand_bit_width);
}

// Lower cast rvalue: convert from source type to target type
auto LowerCastRvalue(
    Context& context, const mir::CastRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  // Lower the source operand
  llvm::Value* source = LowerOperand(context, operands[0]);

  // Check if source is signed for proper extension
  const Type& source_type = types[info.source_type];
  bool is_signed = IsPacked(source_type) && IsPackedSigned(source_type, types);

  // Extend or truncate to target storage type
  if (is_signed) {
    return builder.CreateSExtOrTrunc(source, storage_type, "cast");
  }
  return builder.CreateZExtOrTrunc(source, storage_type, "cast");
}

// Check if type is a packed integral (kIntegral or kPackedArray, not
// kPackedStruct)
auto IsPackedIntegral(const Type& type) -> bool {
  return type.Kind() == TypeKind::kIntegral ||
         type.Kind() == TypeKind::kPackedArray;
}

// Lower bitcast rvalue: reinterpret bits (real <-> integral)
auto LowerBitCastRvalue(
    Context& context, const mir::BitCastRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  llvm::Value* src = LowerOperand(context, operands[0]);
  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  // Case 1: real -> packed integral
  if (src_type.Kind() == TypeKind::kReal && IsPackedIntegral(tgt_type)) {
    return builder.CreateBitCast(
        src, llvm::Type::getInt64Ty(llvm_ctx), "bitcast");
  }

  // Case 2: packed integral -> real
  if (IsPackedIntegral(src_type) && tgt_type.Kind() == TypeKind::kReal) {
    return builder.CreateBitCast(
        src, llvm::Type::getDoubleTy(llvm_ctx), "bitcast");
  }

  // Case 3: shortreal -> packed integral
  if (src_type.Kind() == TypeKind::kShortReal && IsPackedIntegral(tgt_type)) {
    return builder.CreateBitCast(
        src, llvm::Type::getInt32Ty(llvm_ctx), "bitcast");
  }

  // Case 4: packed integral -> shortreal
  if (IsPackedIntegral(src_type) && tgt_type.Kind() == TypeKind::kShortReal) {
    return builder.CreateBitCast(
        src, llvm::Type::getFloatTy(llvm_ctx), "bitcast");
  }

  // HIR guarantees validity - unreachable
  llvm_unreachable("invalid bitcast types");
}

}  // namespace

void LowerCompute(Context& context, const mir::Compute& compute) {
  auto& builder = context.GetBuilder();

  PlaceTypeInfo type_info = ValidateAndGetTypeInfo(context, compute.target);

  if (type_info.is_four_state) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
        context.GetCurrentOrigin(), "4-state types not yet supported");
  }

  llvm::AllocaInst* alloca = context.GetOrCreatePlaceStorage(compute.target);
  llvm::Type* storage_type = alloca->getAllocatedType();

  llvm::Value* result = std::visit(
      Overloaded{
          [&](const mir::UnaryRvalueInfo& info) {
            return LowerUnaryRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::BinaryRvalueInfo& info) {
            return LowerBinaryRvalue(
                context, info, compute.value.operands, storage_type,
                type_info.bit_width);
          },
          [&](const mir::CastRvalueInfo& info) {
            return LowerCastRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::BitCastRvalueInfo& info) {
            return LowerBitCastRvalue(context, info, compute.value.operands);
          },
          [&](const auto& /*info*/) -> llvm::Value* {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                std::format(
                    "unsupported rvalue kind: {}",
                    mir::GetRvalueKind(compute.value.info)));
          },
      },
      compute.value.info);

  result = ApplyWidthMask(context, result, type_info.bit_width);
  builder.CreateStore(result, alloca);
}

}  // namespace lyra::lowering::mir_to_llvm
