#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Per-wait-site entry produced during process codegen.
// Index = wait_site_id (assigned sequentially via Context::NextWaitSiteId).
struct WaitSiteEntry {
  uint32_t resume_block;
  uint32_t num_triggers;
  bool has_late_bound;
  bool has_container;
};

// Output of process code generation.
struct ProcessCodegenResult {
  llvm::Function* function;
  std::vector<WaitSiteEntry> wait_sites;
};

// Generate a complete process function with resume dispatch.
// The function signature is: void process_N(void* state, uint32_t resume_block)
auto GenerateProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> Result<ProcessCodegenResult>;

// Generate a template process function with extra arguments for sharing.
// Signature: void(ptr state, i32 resume, ptr this_ptr, i32 inst_id,
//                 i32 signal_offset)
// this_ptr points to instance storage; the wrapper computes it from
// design_ptr + base_byte_offset. The context must have template-mode fields
// configured before calling.
auto GenerateSharedProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> Result<ProcessCodegenResult>;

// Generate a thin wrapper that calls the template function with baked-in args.
// Computes this_ptr = design_ptr + base_byte_offset, then calls the shared
// template function. Signature: void(ptr state, i32 resume).
auto GenerateProcessWrapper(
    Context& context, llvm::Function* shared_fn, uint32_t instance_id,
    uint64_t base_byte_offset, uint32_t base_slot_id, const std::string& name)
    -> llvm::Function*;

// Declare a user function without generating its body.
// Used for two-pass generation to enable mutual recursion.
auto DeclareUserFunction(
    Context& context, mir::FunctionId func_id, const std::string& name)
    -> Result<llvm::Function*>;

// Generate the body for a user function.
// The function must have been declared first with DeclareUserFunction.
auto DefineUserFunction(
    Context& context, mir::FunctionId func_id, llvm::Function* func)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
