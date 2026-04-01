#pragma once

// Value representation helpers for LLVM type mapping.
//
// These are neutral representation utilities used by both the callable ABI
// layer and the layout/storage layer. They map semantic width / four-state
// properties to LLVM types without coupling to any specific consumer layer.

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

namespace lyra::lowering::mir_to_llvm {

// Backing-domain LLVM integer type for SSA values and allocas.
// For widths <= 64: power-of-2 rounded (i8, i16, i32, i64).
// For widths > 64: exact semantic width.
auto GetBackingLlvmType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type*;

// Backing-domain LLVM struct type for 4-state values: {iN, iN}.
auto GetBackingFourStateType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType*;

}  // namespace lyra::lowering::mir_to_llvm
