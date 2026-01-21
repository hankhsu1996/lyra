#include "lyra/lowering/mir_to_llvm/instruction_compute.hpp"

#include <format>
#include <stdexcept>

#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/mir_to_llvm/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

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

// Validates type is supported and returns {semantic_width, is_four_state}
auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> std::pair<uint32_t, bool> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[place_id];
  const Type& type = types[place.root.type];

  if (!IsPacked(type)) {
    throw std::runtime_error("non-packed types not supported in LLVM backend");
  }
  if (ContainsPackedStruct(type, types)) {
    throw std::runtime_error(
        "packed structs not yet supported in LLVM backend");
  }
  return {PackedBitWidth(type, types), IsPackedFourState(type, types)};
}

// Masks result to semantic width if needed
auto ApplyWidthMask(
    Context& context, llvm::Value* value, uint32_t semantic_width)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  uint32_t storage_width = value->getType()->getIntegerBitWidth();

  if (semantic_width == 0) {
    throw std::runtime_error("semantic width cannot be 0");
  }
  if (semantic_width > storage_width) {
    throw std::runtime_error(
        std::format(
            "semantic width ({}) exceeds storage width ({})", semantic_width,
            storage_width));
  }

  // No masking needed if semantic == storage, or for 64-bit (full word)
  if (semantic_width == storage_width || semantic_width == 64) {
    return value;
  }

  uint64_t mask = (1ULL << semantic_width) - 1;
  return builder.CreateAnd(
      value, llvm::ConstantInt::get(value->getType(), mask), "mask");
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
    default:
      throw std::runtime_error(
          std::format(
              "unsupported binary op in LLVM backend: {}", mir::ToString(op)));
  }
}

// Lower binary rvalue: coerce operands to storage type and compute
auto LowerBinaryRvalue(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  // Lower both operands
  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

  // Coerce both operands to storage type BEFORE operation
  lhs = builder.CreateZExtOrTrunc(lhs, storage_type, "lhs.coerce");
  rhs = builder.CreateZExtOrTrunc(rhs, storage_type, "rhs.coerce");

  return LowerBinaryArith(context, info.op, lhs, rhs);
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

}  // namespace

void LowerCompute(Context& context, const mir::Compute& compute) {
  auto& builder = context.GetBuilder();

  // Step 1: Validate type and get info
  auto [semantic_width, is_four_state] =
      ValidateAndGetTypeInfo(context, compute.target);

  if (is_four_state) {
    throw std::runtime_error("4-state types not yet supported in LLVM backend");
  }
  if (semantic_width > 64) {
    throw std::runtime_error(
        "types wider than 64 bits not yet supported in LLVM backend");
  }

  // Step 2: Get storage for target place
  llvm::AllocaInst* alloca = context.GetOrCreatePlaceStorage(compute.target);
  llvm::Type* storage_type = alloca->getAllocatedType();

  // Step 3: Lower the rvalue
  llvm::Value* result = std::visit(
      Overloaded{
          [&](const mir::BinaryRvalueInfo& info) {
            return LowerBinaryRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::CastRvalueInfo& info) {
            return LowerCastRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [](const auto& /*info*/) -> llvm::Value* {
            throw std::runtime_error("unsupported rvalue kind in LLVM backend");
          },
      },
      compute.value.info);

  // Step 4: Apply width mask
  result = ApplyWidthMask(context, result, semantic_width);

  // Step 5: Store to target
  builder.CreateStore(result, alloca);
}

}  // namespace lyra::lowering::mir_to_llvm
