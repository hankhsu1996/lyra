#pragma once

#include <string>

#include <llvm/IR/Value.h>

#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Emit IR to apply 4-state X-encoding patches using runtime helpers.
// Creates global constant arrays for offsets and masks, then emits calls
// to LyraApply4StatePatches{8,16,32,64} for each non-empty patch group.
//
// This function emits O(1) calls (up to 4) regardless of how many patches
// exist, moving the per-field loop from IR to C++ runtime.
//
// Parameters:
//   ctx: LLVM lowering context
//   base_ptr: pointer to the base of the struct (DesignState or ProcessFrame)
//   patches: the patch table computed at layout time
//   name_prefix: prefix for global variable names (e.g., "design" or "frame.0")
void EmitApply4StatePatches(
    Context& ctx, llvm::Value* base_ptr, const FourStatePatchTable& patches,
    const std::string& name_prefix);

}  // namespace lyra::lowering::mir_to_llvm
