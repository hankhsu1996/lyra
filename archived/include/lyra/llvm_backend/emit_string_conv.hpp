#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace llvm {
class Function;
}

namespace lyra::lowering::mir_to_llvm {

struct CuFacts;

// Convert a packed value to a string handle by calling LyraStringFromPacked.
//
// Behavior:
// - For 4-state values: extracts value plane (unknown bits ignored)
// - Stores value to stack temp, calls runtime with pointer + bit_width
// - Returns a NEWLY ALLOCATED string handle (refcount=1)
//
// Ownership: Caller MUST release the returned handle via LyraStringRelease
// when the temporary string is no longer needed.
auto EmitPackedToString(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* from_packed_fn, llvm::Value* packed_value,
    const Type& packed_type) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
