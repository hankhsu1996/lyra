#pragma once

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include "lyra/common/byte_offset.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct CuFacts;

// Build the canonical LLVM struct type matching LyraRuntimeAbi.
// All codegen that creates or reads the ABI struct must use this function.
// Field numbering is private to runtime_abi_codegen.cpp.
auto GetRuntimeAbiStructType(llvm::LLVMContext& ctx) -> llvm::StructType*;

// Load a specific RuntimeInstance* from the ABI's instance pointer array.
auto EmitLoadAbiInstancePtr(
    Context& context, llvm::Value* abi_ptr, uint32_t instance_id)
    -> llvm::Value*;

// Emit IR that resolves a byte address within an instance's owned storage.
//
// This is the SOLE canonical boundary for walking RuntimeInstance::storage
// in codegen. No other inspection, report, or hook code may GEP into
// RuntimeInstance storage fields directly. All instance-owned byte
// address resolution must go through this helper.
//
// Loads inline_size from RuntimeInstance::storage at runtime to dispatch
// between inline and appendix regions. The caller does NOT pass
// inline_size -- the storage model owns the split.
auto EmitInstanceOwnedByteAddress(
    Context& context, const CuFacts& facts, llvm::Value* instance_ptr,
    common::InstanceByteOffset rel_off) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
