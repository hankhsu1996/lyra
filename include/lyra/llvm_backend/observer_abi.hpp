#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

// Loaded fields from an ObserverContext struct pointer.
struct LoadedObserverContext {
  llvm::Value* this_ptr;
  llvm::Value* instance_id;
  llvm::Value* signal_id_offset;
};

// Get the LLVM struct type for ObserverContext.
// Layout: { ptr this_ptr, i32 instance_id, i32 signal_id_offset }
auto GetObserverContextStructType(llvm::LLVMContext& llvm_ctx)
    -> llvm::StructType*;

// Load all fields from an ObserverContext* value.
auto LoadObserverContextFields(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* observer_ctx_ptr) -> LoadedObserverContext;

// Enter specialization-local lowering mode from an ObserverContext*.
// Loads fields, installs this_ptr/instance_id/signal_id_offset
// and sets kSpecializationLocal addressing mode.
void EnterObserverSpecializationLocalContext(
    Context& context, mir::FunctionId func_id, llvm::Value* observer_ctx_ptr);

// Get observer context field values from current lowering state.
// Returns null/zero defaults for fields not present (design-global callers).
auto GetObserverContextFieldValues(Context& context) -> LoadedObserverContext;

// Allocate an ObserverContext struct on the stack and fill it from
// current lowering state. Uses null/zero defaults for design-global callers.
auto MaterializeObserverContext(Context& context) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
