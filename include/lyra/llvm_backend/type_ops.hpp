#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Determine ownership from source operand (temp = move, persistent = clone)
auto DetermineOwnership(Context& context, const mir::Operand& source)
    -> OwnershipPolicy;

// Main entry point for type-semantic assignment - called from LowerAssign.
// Handles all ownership semantics: string refcount, dynarray clone, struct
// field recursion, etc. Delegates to type-specific handlers internally.
//
// Returns error Diagnostic for unsupported cases (e.g., design slot +
// string struct).
auto AssignPlace(
    Context& context, mir::PlaceId target, const mir::Operand& source)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
