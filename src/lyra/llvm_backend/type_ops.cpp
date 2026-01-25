#include "lyra/llvm_backend/type_ops.hpp"

#include <variant>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/mir/arena.hpp"
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

void AssignPlace(
    Context& context, mir::PlaceId target, const mir::Operand& source) {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Get the effective type (element type if projected, root type otherwise)
  const auto& place = arena[target];
  TypeId type_id = mir::TypeOfPlace(types, place);
  const Type& type = types[type_id];

  OwnershipPolicy policy = DetermineOwnership(context, source);
  llvm::Type* storage_type = context.GetPlaceLlvmType(target);

  // Dispatch based on managed kind first
  switch (GetManagedKind(type.Kind())) {
    case ManagedKind::kString:
      AssignString(context, target, source, policy, type_id);
      return;
    case ManagedKind::kContainer:
      AssignDynArray(context, target, source, policy, type_id);
      return;
    case ManagedKind::kNone:
      break;
  }

  // Non-managed types: dispatch by TypeKind
  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct:
      AssignStruct(context, target, source, policy, type_id);
      return;
    case TypeKind::kUnpackedArray:
      AssignArray(context, target, source, type_id);
      return;
    case TypeKind::kUnpackedUnion:
      AssignUnion(context, target, source, type_id);
      return;
    default:
      break;
  }

  // Packed types: dispatch on LLVM storage type
  if (storage_type->isStructTy()) {
    // 4-state target
    AssignFourState(
        context, target, source, llvm::cast<llvm::StructType>(storage_type));
  } else {
    // 2-state target
    AssignTwoState(context, target, source, type_id);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
