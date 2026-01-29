#include "lyra/llvm_backend/type_ops.hpp"

#include <expected>
#include <variant>

#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto DetermineOwnership(Context& context, const mir::Operand& source)
    -> OwnershipPolicy {
  if (!std::holds_alternative<mir::PlaceId>(source.payload)) {
    // Constants/literals: no ownership transfer
    return OwnershipPolicy::kClone;
  }

  auto src_place_id = std::get<mir::PlaceId>(source.payload);
  const auto& arena = context.GetMirArena();
  const auto& src_place = arena[src_place_id];

  if (src_place.root.kind == mir::PlaceRoot::Kind::kTemp) {
    // MIR invariant: temps are single-owner; Use(temp) is a consuming move
    return OwnershipPolicy::kMove;
  }

  // Persistent places (design, frame) require clone for shared ownership
  return OwnershipPolicy::kClone;
}

auto AssignPlace(
    Context& context, mir::PlaceId target, const mir::Operand& source)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Get the effective type (element type if projected, root type otherwise)
  const auto& place = arena[target];
  TypeId type_id = mir::TypeOfPlace(types, place);
  const Type& type = types[type_id];

  OwnershipPolicy policy = DetermineOwnership(context, source);
  auto storage_type_or_err = context.GetPlaceLlvmType(target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  // Dispatch based on managed kind first
  switch (GetManagedKind(type.Kind())) {
    case ManagedKind::kString: {
      auto result = AssignString(context, target, source, policy, type_id);
      if (!result) return std::unexpected(result.error());
      return {};
    }
    case ManagedKind::kContainer: {
      auto result = AssignDynArray(context, target, source, policy, type_id);
      if (!result) return std::unexpected(result.error());
      return {};
    }
    case ManagedKind::kNone:
      break;
  }

  // Non-managed types: dispatch by TypeKind
  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct: {
      auto result = AssignStruct(context, target, source, policy, type_id);
      if (!result) return std::unexpected(result.error());
      return {};
    }
    case TypeKind::kUnpackedArray: {
      auto result = AssignArray(context, target, source, policy, type_id);
      if (!result) return std::unexpected(result.error());
      return {};
    }
    case TypeKind::kUnpackedUnion: {
      auto result = AssignUnion(context, target, source, type_id);
      if (!result) return std::unexpected(result.error());
      return {};
    }
    default:
      break;
  }

  // Packed types: dispatch on LLVM storage type
  if (storage_type->isStructTy()) {
    // 4-state target
    auto result = AssignFourState(context, target, source, policy, type_id);
    if (!result) return std::unexpected(result.error());
  } else {
    // 2-state target
    auto result = AssignTwoState(context, target, source, policy, type_id);
    if (!result) return std::unexpected(result.error());
  }
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
