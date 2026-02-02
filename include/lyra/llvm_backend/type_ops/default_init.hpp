#pragma once

#include <optional>

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Alignment.h>

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

// Emit memset(dst_ptr, 0, sizeof(pointee_ty)) to zero-initialize storage.
// If align is not provided, infers alignment from the pointer (if alloca)
// or uses the preferred alignment for the type.
void EmitMemsetZero(
    Context& ctx, llvm::Value* dst_ptr, llvm::Type* pointee_ty,
    std::optional<llvm::Align> align = std::nullopt);

}  // namespace lyra::lowering::mir_to_llvm
