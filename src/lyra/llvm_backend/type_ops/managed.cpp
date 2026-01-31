#include "lyra/llvm_backend/type_ops/managed.hpp"

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_queries.hpp"

namespace lyra::lowering::mir_to_llvm {

auto GetManagedKind(TypeKind kind) -> ManagedKind {
  switch (kind) {
    case TypeKind::kString:
      return ManagedKind::kString;
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return ManagedKind::kContainer;
    default:
      return ManagedKind::kNone;
  }
}

// Delegate to common implementation
auto TypeContainsManaged(TypeId type_id, const TypeArena& types) -> bool {
  return common::TypeContainsManaged(type_id, types);
}

auto NeedsFieldByField(TypeId type_id, const TypeArena& types) -> bool {
  // Currently only strings require field-by-field assignment.
  // Container deep-copy is deferred (returns UnsupportedCategory::kType).
  return common::TypeContainsString(type_id, types);
}

auto NeedsDestroy(TypeId type_id, const TypeArena& types) -> bool {
  return common::TypeContainsManaged(type_id, types);
}

}  // namespace lyra::lowering::mir_to_llvm
