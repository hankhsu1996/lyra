#pragma once

#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Ownership policy for assignment - determined once at entry, passed through
enum class OwnershipPolicy { kMove, kClone };

// Determine ownership from source operand (temp = move, persistent = clone)
auto DetermineOwnership(Context& context, const mir::Operand& source)
    -> OwnershipPolicy;

// Main entry point for type-semantic assignment - called from LowerAssign.
// Handles all ownership semantics: string refcount, dynarray clone, struct
// field recursion, etc. Delegates to type-specific handlers internally.
//
// Throws UnsupportedErrorException for deferred cases (e.g., design slot +
// string struct).
void AssignPlace(
    Context& context, mir::PlaceId target, const mir::Operand& source);

}  // namespace lyra::lowering::mir_to_llvm
