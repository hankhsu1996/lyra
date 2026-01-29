#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Emit IR to initialize storage with SV default values.
//
// Default init semantics:
// - 2-state types: 0
// - 4-state types: X (value=0, unknown=semantic_mask)
// - Real types: 0.0
// - Pointer types (string, dynamic array, queue): nullptr
// - Aggregates: recursively apply the above rules
// - Unpacked unions: zero-fill (TODO: proper 4-state union init)
void EmitSVDefaultInit(Context& ctx, llvm::Value* ptr, TypeId type_id);

}  // namespace lyra::lowering::mir_to_llvm
