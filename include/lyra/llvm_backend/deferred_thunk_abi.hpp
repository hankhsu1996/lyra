#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"

namespace lyra::lowering::mir_to_llvm {

// Canonical LLVM struct type for DeferredAssertionExecContext.
// Layout: { ptr instance }
// Single source of truth -- use everywhere the ABI is declared or decoded.
auto BuildDeferredExecContextType(llvm::LLVMContext& ctx) -> llvm::StructType*;

// Build the LLVM struct type for a deferred assertion payload from its
// semantic CapturePayloadDesc. Each field_type is mapped through the
// callable ABI classifier to determine the concrete LLVM storage type.
// Single source of truth -- used by thunk definition (decode) and
// enqueue emission (pack).
auto BuildDeferredPayloadStructType(
    llvm::LLVMContext& llvm_ctx, const mir::CapturePayloadDesc& payload,
    const TypeArena& types, bool force_two_state) -> llvm::StructType*;

}  // namespace lyra::lowering::mir_to_llvm
