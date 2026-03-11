#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct LoweringInput;
struct LoweringResult;

// Per-instance binding data for specialization compilation.
// Stores stable IDs/values; variant is looked up from Layout when needed.
struct SpecInstanceBinding {
  ModuleIndex module_index;
  uint32_t base_slot_id;  // From placement
};

// Specialization-owned MIR content: identity + behavioral IR references.
// Does not contain backend layout or codegen routing data.
struct SpecCompilationUnit {
  mir::ModuleBodyId body_id;
  std::vector<mir::ProcessId> processes;
  std::vector<mir::FunctionId> functions;
  std::vector<SpecInstanceBinding> instances;
};

// Narrow backend view for one body-owned process belonging to a
// specialization. Contains codegen routing data for the process.
struct SpecProcessView {
  uint32_t local_nonfinal_proc_index;  // Position among body's non-final procs
  uint32_t layout_process_index;       // Matching entry in layout.processes
  mir::ProcessId process_id;
  ModuleIndex representative_module_index;  // Compatibility only (%m / path)
  std::string func_name;  // body_{body_id}_proc_{local_nonfinal_proc_index}
};

// Narrow backend view for compiling one specialization body.
// Contains body-owned process routing data.
// Ephemeral: built and consumed inside CompileDesignProcesses, not persisted.
struct SpecCodegenView {
  std::vector<SpecProcessView> processes;
};

// Specialization-local layout data only.
// No design-global state, no wrapper data, no absolute offsets.
// Sourced from Layout::GetBodyRelByteOffsets(body_id).
struct SpecLayout {
  std::vector<uint64_t> rel_byte_offsets;
};

// Specialization compilation input: all data needed to compile one
// specialization body. Owns specialization-local backend data (MIR
// membership, layout, codegen routing). Does not reference orchestrator
// storage -- the specialization compiler reads only from this object.
struct CompiledModuleSpecInput {
  mir::ModuleBodyId body_id;
  std::vector<mir::ProcessId> processes;
  std::vector<mir::FunctionId> functions;
  SpecLayout layout;
  SpecCodegenView view;
};

// Process codegen product of compiling one specialization body.
// Contains process functions and wait sites produced by
// CompileModuleSpecSession. Body-local user functions are registered as
// Context side effects (not returned here) because no downstream consumer
// currently needs them as explicit products.
struct CompiledModuleSpec {
  mir::ModuleBodyId body_id;
  // Parallel to input.view.processes: one compiled function per body process
  std::vector<llvm::Function*> process_functions;
  std::vector<WaitSiteEntry> wait_sites;
};

// Backend-owned intermediate state between behavioral codegen and assembly.
// This is a strict bridge object: assembly may append IR (main()) to the
// module owned by context, but must not own layout/process compilation
// decisions. Assembly-facing helpers live in emit_design_main.cpp.
//
// Layout and Context are heap-allocated because Context stores
// `const Layout&` internally. Returning CodegenSession from
// CompileDesignProcesses via std::expected may move the struct, so both
// must have stable addresses to keep the reference valid.
struct CodegenSession {
  std::unique_ptr<Layout> layout;
  std::unique_ptr<Context> context;
  const mir::Design* design = nullptr;
  std::vector<llvm::Function*> process_funcs;
  std::vector<WaitSiteEntry> wait_sites;
  std::vector<SlotInfo> slot_info;
  size_t num_init_processes = 0;
};

// Backend phase: compile all design processes into LLVM IR.
// Produces a CodegenSession that assembly can append main() into.
auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession>;

// Compile one specialization body: the single native per-specialization backend
// entrypoint. Registers monitors, declares/defines body-local functions,
// generates shared/template process functions, and returns an explicit product.
// Does not inspect design-global state, package functions, or wrapper logic.
auto CompileModuleSpecSession(
    Context& context, const mir::Arena& arena,
    const CompiledModuleSpecInput& input) -> Result<CompiledModuleSpec>;

// Backend phase: extract LLVM ownership from a completed session.
auto FinalizeModule(CodegenSession session) -> LoweringResult;

}  // namespace lyra::lowering::mir_to_llvm
