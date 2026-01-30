#pragma once

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::mir_to_llvm {

// Unified classification of types that require special handling.
// This replaces scattered ContainsStringField/ContainsContainerHandle checks.
enum class ManagedKind {
  kNone,       // POD - no special handling needed
  kString,     // Refcounted string handle
  kContainer,  // DynArray, Queue (future: Assoc)
  // kClass,   // Future: class handles
};

// Get managed kind for a type kind (leaf type check)
auto GetManagedKind(TypeKind kind) -> ManagedKind;

// Check if a type or any nested field/element is managed
auto TypeContainsManaged(TypeId type_id, const TypeArena& types) -> bool;

// Check if type requires field-by-field assignment (contains string fields)
auto NeedsFieldByField(TypeId type_id, const TypeArena& types) -> bool;

// Check if type needs Destroy() to be called (contains any managed type)
auto NeedsDestroy(TypeId type_id, const TypeArena& types) -> bool;

// Check if a return type requires sret calling convention.
// Only VALUE AGGREGATES (unpacked struct/array) use sret.
// Managed HANDLES (string, dynarray, queue) return directly via `ret ptr`.
auto RequiresSret(TypeId type_id, const TypeArena& types) -> bool;

// Check if a type is a managed handle (string, dynarray, queue) at top level.
// These are single-pointer types that can be returned directly.
auto IsManagedHandle(TypeId type_id, const TypeArena& types) -> bool;

}  // namespace lyra::lowering::mir_to_llvm
