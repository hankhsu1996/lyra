#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/constant.hpp"
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

// Specialization-local slot layout classification.
// Computed from raw per-instance relative byte offsets during spec compilation
// setup. Classifies each body-local slot as either:
//   stable: same relative offset across all instances (constant GEP)
//   unstable: offset varies by instance (runtime load from metadata)
//
// Invariant: all instances in a specialization share the same body, so they
// have the same slot set and slot count. The only variation is in relative
// byte offsets (e.g., parameterized unpacked array dimensions change the size
// of a slot, shifting subsequent slots).
//
// This invariant holds because specialization units share one compiled body
// from representative lowering. If body compilation later becomes
// constructor-repertoire-aware (including artifacts from all generate
// branches), this assumption must be revisited.
struct SpecSlotLayout {
  enum class SlotState : uint8_t { kStable, kUnstable };

  std::vector<SlotState> states;
  // Per-slot relative offset for stable slots. UINT64_MAX if not stable.
  std::vector<uint64_t> stable_offsets;
  // Per-slot compact index into the per-instance unstable offset table.
  // UINT32_MAX if not unstable.
  std::vector<uint32_t> unstable_ordinals;
  // Number of unstable slots (= length of each instance's unstable table).
  uint32_t num_unstable = 0;
  // Slot count (same across all instances in the specialization).
  uint32_t num_slots = 0;

  [[nodiscard]] auto IsStable(uint32_t local_slot_id) const -> bool {
    return states[local_slot_id] == SlotState::kStable;
  }
};

// Specialization-local layout data.
// No design-global state, no wrapper data, no absolute offsets.
// Computed from raw per-instance offsets during spec compilation setup.
struct SpecLayout {
  SpecSlotLayout slot_layout;
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

// Parameter initialization entry with pre-resolved type information.
// type_id is intentionally duplicated from the design slot table so that
// realization code does not need a backpointer to mir::Design.
struct ResolvedParamInit {
  uint32_t slot_id;
  TypeId type_id;
  IntegralConstant value;
};

// Design-derived inputs for the realization/assembly phase, extracted during
// CompileDesignProcesses. This is a partial bundle -- only the fields that
// assembly and metadata lowering currently consume. Not the full realization
// model. Indexed forms are explicit so each helper can take narrow views.
struct RealizationData {
  // Indexed by instance table index / instance-slot-range index, matching the
  // existing realization-side per-instance ordering contract.
  std::vector<std::vector<ResolvedParamInit>> param_inits;

  // Indexed by slot_id.value.
  std::vector<TypeId> slot_types;
  std::vector<mir::SlotKind> slot_kinds;

  // Indexed by instance table entry index.
  std::vector<std::string> instance_paths;

  // Compact slot trace provenance from mir::Design (parallel to slot_types).
  std::vector<mir::SlotTraceProvenance> slot_trace_provenance;
  std::vector<char> slot_trace_string_pool;
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
  RealizationData realization;
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
