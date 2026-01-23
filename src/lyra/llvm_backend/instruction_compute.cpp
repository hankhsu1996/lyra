#include "lyra/llvm_backend/instruction_compute.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/ADT/APInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"
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
  const Type& type = types[mir::TypeOfPlace(types, place)];

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
    case mir::BinaryOp::kDivide:
      return builder.CreateUDiv(lhs, rhs, "udiv");
    case mir::BinaryOp::kDivideSigned:
      return builder.CreateSDiv(lhs, rhs, "sdiv");
    case mir::BinaryOp::kMod:
      return builder.CreateURem(lhs, rhs, "urem");
    case mir::BinaryOp::kModSigned:
      return builder.CreateSRem(lhs, rhs, "srem");
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
    case mir::BinaryOp::kCaseZMatch:
    case mir::BinaryOp::kCaseXMatch:
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

// Check if the binary op is a signed comparison (needs sign-extended operands)
auto IsSignedComparisonOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kLessThanSigned:
    case mir::BinaryOp::kLessThanEqualSigned:
    case mir::BinaryOp::kGreaterThanSigned:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      return true;
    default:
      return false;
  }
}

// Sign-extend value from semantic_width to fill storage type.
// Required before signed comparison when storage is wider than semantic width.
auto SignExtendToStorage(
    llvm::IRBuilderBase& builder, llvm::Value* val, uint32_t semantic_width)
    -> llvm::Value* {
  auto* storage_type = val->getType();
  uint32_t storage_width = storage_type->getIntegerBitWidth();
  if (semantic_width >= storage_width) {
    return val;
  }
  auto* sem_ty = llvm::Type::getIntNTy(builder.getContext(), semantic_width);
  auto* truncated = builder.CreateTrunc(val, sem_ty, "sext.trunc");
  return builder.CreateSExt(truncated, storage_type, "sext.ext");
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

auto IsLogicalOp(mir::BinaryOp op) -> bool {
  return op == mir::BinaryOp::kLogicalAnd || op == mir::BinaryOp::kLogicalOr;
}

auto IsCaseMatchOp(mir::BinaryOp op) -> bool {
  return op == mir::BinaryOp::kCaseZMatch || op == mir::BinaryOp::kCaseXMatch;
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

// Unknown plane is a bitmask: right shifts are always logical (LSHR),
// never arithmetic. ASHR would sign-extend the mask (wrong taint geometry).
auto LowerShiftOpUnknown(
    Context& context, mir::BinaryOp op, llvm::Value* unk,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value* {
  mir::BinaryOp unk_op = mir::BinaryOp::kLogicalShiftRight;
  switch (op) {
    case mir::BinaryOp::kLogicalShiftLeft:
    case mir::BinaryOp::kArithmeticShiftLeft:
      unk_op = mir::BinaryOp::kLogicalShiftLeft;
      break;
    default:
      break;
  }
  return LowerShiftOp(context, unk_op, unk, shift_amount, semantic_width);
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

// Resolve the final value type of an operand. For places with projections
// (e.g. arr[i][j]), this returns the element type, not the container type.
auto GetOperandTypeId(Context& context, const mir::Operand& operand) -> TypeId {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      Overloaded{
          [&](const Constant& c) -> TypeId { return c.type; },
          [&](mir::PlaceId place_id) -> TypeId {
            const auto& place = arena[place_id];
            return mir::TypeOfPlace(types, place);
          },
      },
      operand.payload);
}

// Check if an operand has string type.
auto IsStringOperand(Context& context, const mir::Operand& operand) -> bool {
  TypeId type_id = GetOperandTypeId(context, operand);
  return context.GetTypeArena()[type_id].Kind() == TypeKind::kString;
}

// Get the semantic bit width of a packed scalar operand (for sign-extension,
// reduction operators, etc.). Asserts the resolved type is packed.
auto GetOperandPackedWidth(Context& context, const mir::Operand& operand)
    -> uint32_t {
  TypeId type_id = GetOperandTypeId(context, operand);
  const auto& types = context.GetTypeArena();
  return PackedBitWidth(types[type_id], types);
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

auto LowerCaseMatchOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value*;

auto LowerBinaryRvalue(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type,
    uint32_t semantic_width) -> llvm::Value* {
  // Dispatch to string lowering if operands are strings
  if (IsStringOperand(context, operands[0])) {
    return LowerStringBinaryOp(context, info, operands, storage_type);
  }

  // Case match ops need 4-state operands (must dispatch before LowerOperand
  // coerces to 2-state)
  if (IsCaseMatchOp(info.op)) {
    return LowerCaseMatchOp(context, info, operands, storage_type);
  }

  auto& builder = context.GetBuilder();

  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

  // Coerce both operands to storage type BEFORE operation
  lhs = builder.CreateZExtOrTrunc(lhs, storage_type, "lhs.coerce");
  rhs = builder.CreateZExtOrTrunc(rhs, storage_type, "rhs.coerce");

  // Comparison operators use icmp, which returns i1
  if (IsComparisonOp(info.op)) {
    if (IsSignedComparisonOp(info.op)) {
      uint32_t op_width = GetOperandPackedWidth(context, operands[0]);
      lhs = SignExtendToStorage(builder, lhs, op_width);
      rhs = SignExtendToStorage(builder, rhs, op_width);
    }
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
  uint32_t operand_bit_width = GetOperandPackedWidth(context, operands[0]);

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

auto LowerConcatRvalue(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue", "concat must have at least one operand");
  }

  // First operand (MSB): trunc to semantic width, then zext to result type
  uint32_t first_width = GetOperandPackedWidth(context, operands[0]);
  llvm::Value* first = LowerOperand(context, operands[0]);
  auto* first_ty = llvm::Type::getIntNTy(builder.getContext(), first_width);
  first = builder.CreateZExtOrTrunc(first, first_ty, "concat.trunc");
  llvm::Value* acc = builder.CreateZExt(first, storage_type, "concat.ext");

  // Rolling append: acc = (acc << w_next) | zext(trunc(next))
  for (size_t i = 1; i < operands.size(); ++i) {
    uint32_t op_width = GetOperandPackedWidth(context, operands[i]);
    llvm::Value* op = LowerOperand(context, operands[i]);

    auto* op_ty = llvm::Type::getIntNTy(builder.getContext(), op_width);
    op = builder.CreateZExtOrTrunc(op, op_ty, "concat.trunc");
    op = builder.CreateZExt(op, storage_type, "concat.ext");

    auto* shift_amount = llvm::ConstantInt::get(storage_type, op_width);
    acc = builder.CreateShl(acc, shift_amount, "concat.shl");
    acc = builder.CreateOr(acc, op, "concat.or");
  }

  return acc;
}

struct FourStateValue {
  llvm::Value* value;    // Value/Z bits
  llvm::Value* unknown;  // Unknown bits (0 = known)
};

// Extract (value, unknown) from a loaded struct value
auto ExtractFourState(llvm::IRBuilderBase& builder, llvm::Value* struct_val)
    -> FourStateValue {
  auto* val = builder.CreateExtractValue(struct_val, 0, "fs.val");
  auto* unk = builder.CreateExtractValue(struct_val, 1, "fs.unk");
  return {.value = val, .unknown = unk};
}

// Pack (value, unknown) into a struct for storing
auto PackFourState(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val, llvm::Value* unk) -> llvm::Value* {
  llvm::Value* result = llvm::UndefValue::get(struct_type);
  result = builder.CreateInsertValue(result, val, 0, "fs.pack.val");
  result = builder.CreateInsertValue(result, unk, 1, "fs.pack.unk");
  return result;
}

// Check if an operand is 4-state (TypeId-driven)
auto IsOperandFourState(Context& context, const mir::Operand& operand) -> bool {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      Overloaded{
          [&](const Constant& c) {
            const Type& type = types[c.type];
            return IsPacked(type) && IsPackedFourState(type, types);
          },
          [&](mir::PlaceId place_id) {
            const auto& place = arena[place_id];
            TypeId type_id = mir::TypeOfPlace(types, place);
            const Type& type = types[type_id];
            return IsPacked(type) && IsPackedFourState(type, types);
          },
      },
      operand.payload);
}

// Load an operand as FourStateValue.
// For 4-state operands: extracts (a, b) from struct via raw load.
// For 2-state operands: a = loaded value, b = zero constant.
auto LowerOperandFourState(
    Context& context, const mir::Operand& operand, llvm::Type* elem_type)
    -> FourStateValue {
  auto& builder = context.GetBuilder();

  if (IsOperandFourState(context, operand)) {
    // Load raw struct value (bypass 2-state coercion)
    llvm::Value* loaded = LowerOperandRaw(context, operand);
    return ExtractFourState(builder, loaded);
  }

  // 2-state: use LowerOperand (returns integer), coerce to elem_type
  llvm::Value* loaded_val = LowerOperand(context, operand);
  auto* val = builder.CreateZExtOrTrunc(loaded_val, elem_type, "fs2.val");
  auto* unk = llvm::ConstantInt::get(elem_type, 0);
  return {.value = val, .unknown = unk};
}

auto LowerCaseMatchOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  uint32_t operand_width = GetOperandPackedWidth(context, operands[0]);
  auto* elem_type =
      llvm::Type::getIntNTy(context.GetLlvmContext(), operand_width);

  auto lhs = LowerOperandFourState(context, operands[0], elem_type);
  auto rhs = LowerOperandFourState(context, operands[1], elem_type);

  lhs.value = builder.CreateZExtOrTrunc(lhs.value, elem_type, "cm.lhs.val");
  lhs.unknown = builder.CreateZExtOrTrunc(lhs.unknown, elem_type, "cm.lhs.unk");
  rhs.value = builder.CreateZExtOrTrunc(rhs.value, elem_type, "cm.rhs.val");
  rhs.unknown = builder.CreateZExtOrTrunc(rhs.unknown, elem_type, "cm.rhs.unk");

  // Compute wildcard mask
  llvm::Value* wildcard = nullptr;
  if (info.op == mir::BinaryOp::kCaseXMatch) {
    wildcard = builder.CreateOr(lhs.unknown, rhs.unknown, "cx.wc");
  } else {
    auto* lhs_z = builder.CreateAnd(lhs.value, lhs.unknown, "cz.lhs.z");
    auto* rhs_z = builder.CreateAnd(rhs.value, rhs.unknown, "cz.rhs.z");
    wildcard = builder.CreateOr(lhs_z, rhs_z, "cz.wc");
  }

  // Compare non-wildcard bits
  auto* not_wildcard = builder.CreateNot(wildcard, "cm.not.wc");
  auto sem_mask = llvm::APInt::getLowBitsSet(
      elem_type->getIntegerBitWidth(), operand_width);
  auto* mask = builder.CreateAnd(
      not_wildcard, llvm::ConstantInt::get(elem_type, sem_mask), "cm.mask");
  auto* lhs_masked = builder.CreateAnd(lhs.value, mask, "cm.lv");
  auto* rhs_masked = builder.CreateAnd(rhs.value, mask, "cm.rv");
  llvm::Value* result = builder.CreateICmpEQ(lhs_masked, rhs_masked, "cm.veq");

  if (info.op == mir::BinaryOp::kCaseZMatch) {
    auto* lhs_unk_m = builder.CreateAnd(lhs.unknown, mask, "cz.lu");
    auto* rhs_unk_m = builder.CreateAnd(rhs.unknown, mask, "cz.ru");
    auto* unk_eq = builder.CreateICmpEQ(lhs_unk_m, rhs_unk_m, "cz.ueq");
    result = builder.CreateAnd(result, unk_eq, "cz.match");
  }

  return builder.CreateZExt(result, storage_type, "cm.ext");
}

// 4-state concat: same shift+OR algorithm as 2-state, applied to both planes
auto LowerConcatRvalue4State(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type)
    -> FourStateValue {
  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue4State", "concat must have at least one operand");
  }

  // First operand (MSB)
  uint32_t first_width = GetOperandPackedWidth(context, operands[0]);
  auto first = LowerOperandFourState(context, operands[0], elem_type);

  // Trunc to semantic width, then zext to result type
  auto* first_ty = llvm::Type::getIntNTy(builder.getContext(), first_width);
  auto* val_first =
      builder.CreateZExtOrTrunc(first.value, first_ty, "cat4.val.trunc");
  auto* unk_first =
      builder.CreateZExtOrTrunc(first.unknown, first_ty, "cat4.unk.trunc");
  auto* acc_val = builder.CreateZExt(val_first, elem_type, "cat4.val.ext");
  auto* acc_unk = builder.CreateZExt(unk_first, elem_type, "cat4.unk.ext");

  // Rolling append: acc = (acc << w_next) | zext(trunc(next))
  for (size_t i = 1; i < operands.size(); ++i) {
    uint32_t op_width = GetOperandPackedWidth(context, operands[i]);
    auto op = LowerOperandFourState(context, operands[i], elem_type);

    auto* op_ty = llvm::Type::getIntNTy(builder.getContext(), op_width);
    auto* val_op = builder.CreateZExtOrTrunc(op.value, op_ty, "cat4.val.trunc");
    auto* unk_op =
        builder.CreateZExtOrTrunc(op.unknown, op_ty, "cat4.unk.trunc");
    val_op = builder.CreateZExt(val_op, elem_type, "cat4.val.ext");
    unk_op = builder.CreateZExt(unk_op, elem_type, "cat4.unk.ext");

    auto* shift_amount = llvm::ConstantInt::get(elem_type, op_width);
    acc_val = builder.CreateShl(acc_val, shift_amount, "cat4.val.shl");
    acc_val = builder.CreateOr(acc_val, val_op, "cat4.val.or");
    acc_unk = builder.CreateShl(acc_unk, shift_amount, "cat4.unk.shl");
    acc_unk = builder.CreateOr(acc_unk, unk_op, "cat4.unk.or");
  }

  return {.value = acc_val, .unknown = acc_unk};
}

// 4-state cast (pure width-adjust): zext/trunc both planes
auto LowerCastRvalue4State(
    Context& context, const mir::CastRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type)
    -> FourStateValue {
  auto& builder = context.GetBuilder();
  auto src = LowerOperandFourState(context, operands[0], elem_type);
  return {
      .value = builder.CreateZExtOrTrunc(src.value, elem_type, "cast4.val"),
      .unknown = builder.CreateZExtOrTrunc(src.unknown, elem_type, "cast4.unk"),
  };
}

auto LowerUnaryRvalue4State(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type,
    uint32_t semantic_width) -> FourStateValue {
  auto& builder = context.GetBuilder();
  auto* zero = llvm::ConstantInt::get(elem_type, 0);

  auto src = LowerOperandFourState(context, operands[0], elem_type);
  src.value = builder.CreateZExtOrTrunc(src.value, elem_type, "un4.val");
  src.unknown = builder.CreateZExtOrTrunc(src.unknown, elem_type, "un4.unk");

  switch (info.op) {
    case mir::UnaryOp::kPlus:
      return src;

    case mir::UnaryOp::kMinus: {
      // Carry propagation: any unknown bit can affect all higher bits
      auto* any_unk = builder.CreateICmpNE(src.unknown, zero, "un4.anyunk");
      auto* sem_mask = GetSemanticMask(elem_type, semantic_width);
      return {
          .value = builder.CreateNeg(src.value, "un4.neg"),
          .unknown =
              builder.CreateSelect(any_unk, sem_mask, zero, "un4.neg.unk"),
      };
    }

    case mir::UnaryOp::kBitwiseNot:
      // Each bit independent: unknown unchanged
      return {
          .value = builder.CreateNot(src.value, "un4.not"),
          .unknown = src.unknown,
      };

    case mir::UnaryOp::kLogicalNot: {
      // Operate on known bits, taint if any unknown
      auto* known = builder.CreateAnd(
          src.value, builder.CreateNot(src.unknown), "un4.known");
      auto* is_nonzero = builder.CreateICmpNE(known, zero, "un4.nz");
      auto* negated = builder.CreateNot(is_nonzero, "un4.lnot");
      auto* any_unk = builder.CreateICmpNE(src.unknown, zero, "un4.anyunk");
      return {
          .value = builder.CreateZExt(negated, elem_type, "un4.lnot.val"),
          .unknown = builder.CreateZExt(any_unk, elem_type, "un4.lnot.unk"),
      };
    }

    default: {
      // Reductions: mask to semantic width, compute on known bits, taint result
      auto* masked_val = ApplyWidthMask(context, src.value, semantic_width);
      auto* masked_unk = ApplyWidthMask(context, src.unknown, semantic_width);
      auto* known = builder.CreateAnd(
          masked_val, builder.CreateNot(masked_unk), "un4.red.known");

      auto* red_result =
          LowerUnaryOp(context, info.op, known, elem_type, semantic_width);

      auto* any_unk = builder.CreateICmpNE(masked_unk, zero, "un4.red.taint");
      return {
          .value = red_result,
          .unknown = builder.CreateZExt(any_unk, elem_type, "un4.red.unk"),
      };
    }
  }
}

auto LowerIndexValidity(
    Context& context, const mir::IndexValidityRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  // Lower the index operand
  llvm::Value* index = LowerOperand(context, operands[0]);
  auto* idx_type = index->getType();

  // lower_bound <= index (signed)
  auto* lower = llvm::ConstantInt::get(idx_type, info.lower_bound, true);
  auto* ge_lower = builder.CreateICmpSGE(index, lower, "idx.ge_lower");

  // index <= upper_bound (signed)
  auto* upper = llvm::ConstantInt::get(idx_type, info.upper_bound, true);
  auto* le_upper = builder.CreateICmpSLE(index, upper, "idx.le_upper");

  llvm::Value* valid = builder.CreateAnd(ge_lower, le_upper, "idx.valid");

  // For 4-state indices: also check unknown bits are zero
  if (info.check_known) {
    llvm::Value* raw = LowerOperandRaw(context, operands[0]);
    if (raw->getType()->isStructTy()) {
      auto* unk = builder.CreateExtractValue(raw, 1, "idx.unk");
      auto* zero = llvm::ConstantInt::get(unk->getType(), 0);
      auto* is_known = builder.CreateICmpEQ(unk, zero, "idx.is_known");
      valid = builder.CreateAnd(valid, is_known, "idx.valid_known");
    }
  }

  return builder.CreateZExt(valid, storage_type, "idx.ext");
}

auto LowerGuardedUse(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  // Lower validity predicate to i1
  llvm::Value* valid = LowerOperand(context, operands[0]);
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "gu.tobool");
  }

  // Create branch structure: valid → read, invalid → OOB default
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_read_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu.read", func);
  auto* oob_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu.oob", func);
  auto* merge_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu.merge", func);

  builder.CreateCondBr(valid, do_read_bb, oob_bb);

  // Valid path: read from place (may involve BitRangeProjection shift+trunc)
  builder.SetInsertPoint(do_read_bb);
  auto place_operand = mir::Operand::Use(info.place);
  llvm::Value* read_val = LowerOperand(context, place_operand);
  read_val = builder.CreateZExtOrTrunc(read_val, storage_type, "gu.fit");
  auto* do_read_end_bb = builder.GetInsertBlock();
  builder.CreateBr(merge_bb);

  // OOB path: 2-state default is 0
  builder.SetInsertPoint(oob_bb);
  llvm::Value* oob_val = llvm::ConstantInt::get(storage_type, 0);
  builder.CreateBr(merge_bb);

  // Merge
  builder.SetInsertPoint(merge_bb);
  auto* phi = builder.CreatePHI(storage_type, 2, "gu.result");
  phi->addIncoming(read_val, do_read_end_bb);
  phi->addIncoming(oob_val, oob_bb);
  return phi;
}

auto LowerGuardedUse4State(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type,
    uint32_t semantic_width) -> FourStateValue {
  auto& builder = context.GetBuilder();

  // Lower validity predicate to i1
  llvm::Value* valid = LowerOperand(context, operands[0]);
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "gu4.tobool");
  }

  // Create branch structure
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_read_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu4.read", func);
  auto* oob_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu4.oob", func);
  auto* merge_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu4.merge", func);

  builder.CreateCondBr(valid, do_read_bb, oob_bb);

  // Valid path: load as 4-state
  builder.SetInsertPoint(do_read_bb);
  auto place_operand = mir::Operand::Use(info.place);
  auto read_fs = LowerOperandFourState(context, place_operand, elem_type);
  auto* do_read_end_bb = builder.GetInsertBlock();
  builder.CreateBr(merge_bb);

  // OOB path: {value=0, unknown=semantic_mask} (X for semantic bits)
  builder.SetInsertPoint(oob_bb);
  auto* oob_val = llvm::ConstantInt::get(elem_type, 0);
  uint32_t storage_width = elem_type->getIntegerBitWidth();
  auto oob_unk_ap = llvm::APInt::getLowBitsSet(storage_width, semantic_width);
  auto* oob_unk = llvm::ConstantInt::get(elem_type, oob_unk_ap);
  builder.CreateBr(merge_bb);

  // Merge with PHI nodes for both planes
  builder.SetInsertPoint(merge_bb);
  auto* phi_val = builder.CreatePHI(elem_type, 2, "gu4.val");
  phi_val->addIncoming(read_fs.value, do_read_end_bb);
  phi_val->addIncoming(oob_val, oob_bb);

  auto* phi_unk = builder.CreatePHI(elem_type, 2, "gu4.unk");
  phi_unk->addIncoming(read_fs.unknown, do_read_end_bb);
  phi_unk->addIncoming(oob_unk, oob_bb);

  return {.value = phi_val, .unknown = phi_unk};
}

auto LowerBinaryRvalue4State(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type,
    uint32_t semantic_width) -> FourStateValue {
  auto& builder = context.GetBuilder();
  auto* zero = llvm::ConstantInt::get(elem_type, 0);

  auto lhs = LowerOperandFourState(context, operands[0], elem_type);
  auto rhs = LowerOperandFourState(context, operands[1], elem_type);

  lhs.value = builder.CreateZExtOrTrunc(lhs.value, elem_type, "bin4.lhs.val");
  rhs.value = builder.CreateZExtOrTrunc(rhs.value, elem_type, "bin4.rhs.val");
  lhs.unknown =
      builder.CreateZExtOrTrunc(lhs.unknown, elem_type, "bin4.lhs.unk");
  rhs.unknown =
      builder.CreateZExtOrTrunc(rhs.unknown, elem_type, "bin4.rhs.unk");

  auto* combined_unk = builder.CreateOr(lhs.unknown, rhs.unknown, "bin4.unk");

  if (IsComparisonOp(info.op)) {
    auto* cmp_lhs = lhs.value;
    auto* cmp_rhs = rhs.value;
    if (IsSignedComparisonOp(info.op)) {
      uint32_t op_width = GetOperandPackedWidth(context, operands[0]);
      cmp_lhs = SignExtendToStorage(builder, cmp_lhs, op_width);
      cmp_rhs = SignExtendToStorage(builder, cmp_rhs, op_width);
    }
    auto* cmp = LowerBinaryComparison(context, info.op, cmp_lhs, cmp_rhs);
    auto* taint = builder.CreateICmpNE(combined_unk, zero, "bin4.taint");
    return {
        .value = builder.CreateZExt(cmp, elem_type, "bin4.cmp.val"),
        .unknown = builder.CreateZExt(taint, elem_type, "bin4.cmp.unk"),
    };
  }

  if (IsLogicalOp(info.op)) {
    auto* val = LowerBinaryArith(context, info.op, lhs.value, rhs.value);
    auto* taint = builder.CreateICmpNE(combined_unk, zero, "bin4.taint");
    return {
        .value = builder.CreateZExt(val, elem_type, "bin4.log.val"),
        .unknown = builder.CreateZExt(taint, elem_type, "bin4.log.unk"),
    };
  }

  if (IsShiftOp(info.op)) {
    auto* val =
        LowerShiftOp(context, info.op, lhs.value, rhs.value, semantic_width);
    auto* unk = LowerShiftOpUnknown(
        context, info.op, lhs.unknown, rhs.value, semantic_width);
    return {.value = val, .unknown = unk};
  }

  // Arithmetic/bitwise — propagate combined taint
  if (ReturnsI1(info.op)) {
    throw common::InternalError(
        "LowerBinaryRvalue4State",
        "i1-producing op must be handled as comparison or logical");
  }
  auto* val = LowerBinaryArith(context, info.op, lhs.value, rhs.value);
  return {.value = val, .unknown = combined_unk};
}

}  // namespace

void LowerCompute(Context& context, const mir::Compute& compute) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  // Unpacked array aggregate construction: early-exit before
  // ValidateAndGetTypeInfo
  if (std::holds_alternative<mir::AggregateRvalueInfo>(compute.value.info)) {
    const auto& arena = context.GetMirArena();
    const Type& target_type =
        types[mir::TypeOfPlace(types, arena[compute.target])];
    if (target_type.Kind() == TypeKind::kUnpackedArray) {
      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* arr_type = context.GetPlaceLlvmType(compute.target);
      llvm::Type* elem_type = arr_type->getArrayElementType();

      llvm::Value* aggregate = llvm::UndefValue::get(arr_type);
      for (size_t i = 0; i < compute.value.operands.size(); ++i) {
        llvm::Value* elem = LowerOperandAsStorage(
            context, compute.value.operands[i], elem_type);
        aggregate = builder.CreateInsertValue(
            aggregate, elem, {static_cast<unsigned>(i)});
      }
      builder.CreateStore(aggregate, target_ptr);
      return;
    }
  }

  PlaceTypeInfo type_info = ValidateAndGetTypeInfo(context, compute.target);

  // Get storage for target place
  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(compute.target);

  if (type_info.is_four_state) {
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    auto* elem_type = struct_type->getElementType(0);

    FourStateValue result = std::visit(
        Overloaded{
            [&](const mir::ConcatRvalueInfo& info) {
              return LowerConcatRvalue4State(
                  context, info, compute.value.operands, elem_type);
            },
            [&](const mir::CastRvalueInfo& info) {
              return LowerCastRvalue4State(
                  context, info, compute.value.operands, elem_type);
            },
            [&](const mir::UnaryRvalueInfo& info) {
              return LowerUnaryRvalue4State(
                  context, info, compute.value.operands, elem_type,
                  type_info.bit_width);
            },
            [&](const mir::BinaryRvalueInfo& info) {
              return LowerBinaryRvalue4State(
                  context, info, compute.value.operands, elem_type,
                  type_info.bit_width);
            },
            [&](const mir::GuardedUseRvalueInfo& info) {
              return LowerGuardedUse4State(
                  context, info, compute.value.operands, elem_type,
                  type_info.bit_width);
            },
            [&](const auto& /*info*/) -> FourStateValue {
              throw common::UnsupportedErrorException(
                  common::UnsupportedLayer::kMirToLlvm,
                  common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                  std::format(
                      "4-state rvalue kind not yet supported: {}",
                      mir::GetRvalueKind(compute.value.info)));
            },
        },
        compute.value.info);

    // INVARIANT: semantic mask on both components
    result.value = ApplyWidthMask(context, result.value, type_info.bit_width);
    result.unknown =
        ApplyWidthMask(context, result.unknown, type_info.bit_width);

    llvm::Value* packed =
        PackFourState(builder, struct_type, result.value, result.unknown);
    builder.CreateStore(packed, target_ptr);
    return;
  }

  // 2-state path (unchanged)
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
          [&](const mir::ConcatRvalueInfo& info) {
            return LowerConcatRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::IndexValidityRvalueInfo& info) {
            return LowerIndexValidity(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::GuardedUseRvalueInfo& info) {
            return LowerGuardedUse(
                context, info, compute.value.operands, storage_type);
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

  // Apply width mask
  result = ApplyWidthMask(context, result, type_info.bit_width);

  // Store to target
  builder.CreateStore(result, target_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm
