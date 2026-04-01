#include "lyra/llvm_backend/callable_abi.hpp"

#include <format>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/layout/layout_four_state.hpp"
#include "lyra/llvm_backend/value_repr.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ClassifyCallableValueAbi(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> std::optional<CallableValueAbiInfo> {
  const auto& type = types[type_id];

  switch (type.Kind()) {
    // Pointer-valued types: passed and returned as opaque pointer directly.
    // This class covers both managed handles (string, dynamic containers)
    // and unmanaged opaque handles (chandle). The distinction between
    // managed and unmanaged is a runtime ownership concern, not a callable
    // ABI concern -- both are transported as ptr at the boundary.
    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
    case TypeKind::kChandle:
      return CallableValueAbiInfo{
          .repr_class = CallableValueReprClass::kPointerValue,
          .llvm_type = llvm::PointerType::getUnqual(ctx),
      };

    case TypeKind::kReal:
      return CallableValueAbiInfo{
          .repr_class = CallableValueReprClass::kReal,
          .llvm_type = llvm::Type::getDoubleTy(ctx),
      };

    case TypeKind::kShortReal:
      return CallableValueAbiInfo{
          .repr_class = CallableValueReprClass::kShortReal,
          .llvm_type = llvm::Type::getFloatTy(ctx),
      };

    // Packed integral types: 2-state or 4-state backed.
    // chandle must never reach this path -- it is pointer-valued, not packed.
    case TypeKind::kIntegral:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t width = PackedBitWidth(type, types);
      if (IsLayoutPackedFourState(type, types, force_two_state)) {
        return CallableValueAbiInfo{
            .repr_class = CallableValueReprClass::kPackedFourState,
            .llvm_type = GetBackingFourStateType(ctx, width),
        };
      }
      return CallableValueAbiInfo{
          .repr_class = CallableValueReprClass::kPackedTwoState,
          .llvm_type = GetBackingLlvmType(ctx, width),
      };
    }

    // Aggregates: not passable by value in the internal callable ABI.
    case TypeKind::kUnpackedArray:
    case TypeKind::kUnpackedStruct:
    case TypeKind::kUnpackedUnion:
      return std::nullopt;

    case TypeKind::kVoid:
      throw common::InternalError(
          "ClassifyCallableValueAbi",
          std::format(
              "void type {} must not reach callable value classification",
              type_id.value));
  }

  throw common::InternalError(
      "ClassifyCallableValueAbi",
      std::format(
          "type kind {} for type {} is not representable in the internal "
          "callable ABI",
          static_cast<int>(type.Kind()), type_id.value));
}

auto GetCallableAbiLlvmType(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> llvm::Type* {
  auto info = ClassifyCallableValueAbi(ctx, type_id, types, force_two_state);
  if (!info) return nullptr;
  return info->llvm_type;
}

}  // namespace lyra::lowering::mir_to_llvm
