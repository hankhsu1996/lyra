#pragma once

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/common/type_utils.hpp"

namespace lyra::lowering::mir_to_llvm {

// Backend-local wrappers for 4-state type queries.
//
// All LLVM backend code must use these wrappers (or Context::IsFourState /
// Context::IsPackedFourState) instead of the common:: free functions
// (IsFourStateType, IsPackedFourState) or direct field access (.is_four_state).
// This allows --two-state mode to force all types to be treated as 2-state
// during LLVM lowering without changing the type system.
//
// Non-context overloads: for layout phase (before Context exists).
// Context-based overloads: on Context class (for lowering phase).
//
// Enforced by tools/policy/check_llvm_backend_boundaries.py.

inline auto IsFourState(
    TypeId tid, const TypeArena& types, bool force_two_state) -> bool {
  if (force_two_state) return false;
  return IsFourStateType(tid, types);
}

inline auto IsPackedFourState(
    const Type& type, const TypeArena& types, bool force_two_state) -> bool {
  if (force_two_state) return false;
  return lyra::IsPackedFourState(type, types);
}

}  // namespace lyra::lowering::mir_to_llvm
