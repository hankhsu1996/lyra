#pragma once

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
void EmitApply4StatePatches(
    Context& ctx, llvm::Value* base_ptr, const FourStatePatchTable& patches);

}  // namespace lyra::lowering::mir_to_llvm
