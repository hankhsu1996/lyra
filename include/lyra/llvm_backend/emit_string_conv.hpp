#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

// Convert a packed value to a string handle by calling LyraStringFromPacked.
//
// Behavior:
// - For 4-state values: extracts value plane (unknown bits ignored)
// - Stores value to stack temp, calls runtime with pointer + bit_width
// - Returns a NEWLY ALLOCATED string handle (refcount=1)
//
// Ownership: Caller MUST release the returned handle via LyraStringRelease
// when the temporary string is no longer needed.
//
// Parameters:
// - context: LLVM lowering context
// - packed_value: The packed value (iN or {iN, iN} for 4-state)
// - packed_type: The Type of the packed value (used for bit_width and 4-state
//                check). Must be a packed type (asserted in debug builds).
//
// Returns: ptr to newly allocated string handle
auto EmitPackedToString(
    Context& context, llvm::Value* packed_value, const Type& packed_type)
    -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
