#include "lyra/llvm_backend/type_ops/managed.hpp"

#include <algorithm>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

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

namespace {

// Recursively check if type (or any nested field/element) satisfies predicate
template <typename Pred>
auto TypeContains(TypeId type_id, const TypeArena& types, Pred pred) -> bool {
  const Type& type = types[type_id];
  if (pred(type.Kind())) {
    return true;
  }

  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      return std::ranges::any_of(info.fields, [&](const auto& field) {
        return TypeContains(field.type, types, pred);
      });
    }
    case TypeKind::kUnpackedArray:
      return TypeContains(type.AsUnpackedArray().element_type, types, pred);
    default:
      return false;
  }
}

}  // namespace

auto TypeContainsManaged(TypeId type_id, const TypeArena& types) -> bool {
  return TypeContains(type_id, types, [](TypeKind kind) {
    return GetManagedKind(kind) != ManagedKind::kNone;
  });
}

auto NeedsFieldByField(TypeId type_id, const TypeArena& types) -> bool {
  // Currently only strings require field-by-field assignment.
  // Container deep-copy is deferred (returns UnsupportedCategory::kType).
  return TypeContains(
      type_id, types, [](TypeKind kind) { return kind == TypeKind::kString; });
}

auto NeedsDestroy(TypeId type_id, const TypeArena& types) -> bool {
  return TypeContainsManaged(type_id, types);
}

auto RequiresSret(TypeId type_id, const TypeArena& types) -> bool {
  const auto& type = types[type_id];
  // All aggregates require sret (out-param calling convention)
  if (type.Kind() == TypeKind::kUnpackedStruct) return true;
  if (type.Kind() == TypeKind::kUnpackedArray) return true;
  // Managed types (string, containers) also require sret
  return NeedsDestroy(type_id, types);
}

}  // namespace lyra::lowering::mir_to_llvm
