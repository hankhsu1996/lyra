#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lowering_reports.hpp"
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
  uint32_t signal_id_offset = 0;
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
// specialization. Contains codegen routing data for shared process compilation.
// Does not carry instance identity -- dynamic instance identity flows through
// wrapper/runtime inputs, not through static codegen state.
struct SpecProcessView {
  uint32_t nonfinal_proc_ordinal;  // Dense non-final body-local process ordinal
  uint32_t layout_process_index;   // Matching entry in layout.processes
  mir::ProcessId process_id;
  std::string func_name;  // body_{body_id}_proc_{nonfinal_proc_ordinal}
};

// Narrow backend view for compiling one specialization body.
// Contains body-owned process routing data.
// Ephemeral: built and consumed inside CompileDesignProcesses, not persisted.
struct SpecCodegenView {
  std::vector<SpecProcessView> processes;
};

// Per-specialization slot info for owned-container dispatch.
// All inline offsets are stable (owned slots are fixed-size handles).
// Used by body codegen to dispatch between inline and owned access paths.
struct SpecSlotInfo {
  // Body identity for this specialization. Used by
  // Context::GetCurrentBodyRealizationInfo to resolve body-relative
  // metadata (behavioral trigger contract, etc.) from Layout.
  mir::ModuleBodyId body_id;
  // Stable index into Layout::body_realization_infos for this body.
  // Set during BuildSpecSlotInfos. Used by GetCurrentBodyRealizationInfo
  // for O(1) lookup without pointer-stability assumptions.
  static constexpr uint32_t kInvalidBodyInfoIndex = UINT32_MAX;
  uint32_t body_realization_info_index = kInvalidBodyInfoIndex;

  // Per-slot inline-region relative offset from this_ptr.
  // For kInlineValue: offset of the slot's value bytes.
  // For kOwnedContainer: offset of the OwnedStorageHandle.
  std::vector<uint64_t> inline_offsets;
  // Per-slot storage shape.
  std::vector<mir::StorageShape> shapes;
  // Per-slot representative design-global slot index. Used to resolve
  // compile-time metadata (e.g., element stride from ArrayStorageSpec).
  // Valid because storage shape is specialization-invariant: all instances
  // in the specialization share the same shape per body-local slot.
  std::vector<uint32_t> representative_design_slots;

  [[nodiscard]] auto SlotCount() const -> uint32_t {
    return static_cast<uint32_t>(shapes.size());
  }
  // Unchecked predicate. Callers must validate id < SlotCount() first.
  [[nodiscard]] auto IsOwnedContainer(uint32_t id) const -> bool {
    return shapes[id] == mir::StorageShape::kOwnedContainer;
  }
};

// Specialization compilation input: all data needed to compile one
// specialization body. Owns specialization-local backend data (MIR
// membership, layout, codegen routing). Does not reference orchestrator
// storage -- the specialization compiler reads only from this object.
struct CompiledModuleSpecInput {
  mir::ModuleBodyId body_id;
  std::vector<mir::ProcessId> processes;
  std::vector<mir::FunctionId> functions;
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
  // Parallel to process_functions: one optional trigger entry per body
  // process. Index by body process ordinal to get trigger facts.
  // scheduled_process_index is NOT yet set (stamped per-instance later).
  std::vector<std::optional<ProcessTriggerEntry>> process_triggers;
};

// Design-derived inputs for the realization/assembly phase, extracted during
// CompileDesignProcesses. This is a partial bundle -- only the fields that
// assembly and metadata lowering currently consume. Not the full realization
// model. Indexed forms are explicit so each helper can take narrow views.
struct RealizationData {
  // Per-instance pre-lowered parameter value payloads.
  // Indexed by instance table index. Each entry contains canonical storage
  // bytes for all param slots in body-template order.
  std::vector<std::vector<uint8_t>> param_payloads;

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
  // Per-body compiled process functions for body
  // realization descriptor emission. Parallel to
  // layout->body_realization_infos: entry [i] corresponds to
  // body_realization_infos[i]. Each inner vector is parallel to the
  // body's non-final process list.
  struct BodyCompiledFuncs {
    mir::ModuleBodyId body_id;
    std::vector<llvm::Function*> functions;
  };
  std::vector<BodyCompiledFuncs> body_compiled_funcs;
  // Compile-owned topology summary: per-instance body group index.
  // Indexed by ModuleIndex (instance order). Each entry is the index
  // into body_compiled_funcs / layout->body_realization_infos for that
  // instance. This is retained compile-owned topology summary, not a
  // constructor-side artifact. Used by the emitted constructor function
  // to emit calls in instance-major order (matching old process ordering).
  // Not the long-term end-state (H6/H7 concern).
  std::vector<uint32_t> instance_body_group;
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

// Extract design-derived realization data from narrow inputs.
// Setup helper: called from CompileDesignProcesses during orchestration,
// not part of per-spec compilation. Declared here because RealizationData
// (its return type) is defined in this header.
auto ExtractRealizationData(
    const mir::PlacementMap& placement, std::span<const mir::SlotDesc> slots,
    const mir::InstanceTable& instance_table,
    std::span<const mir::SlotTraceProvenance> slot_trace_provenance,
    std::span<const char> slot_trace_string_pool) -> RealizationData;

// Backend phase: extract LLVM ownership from a completed session.
auto FinalizeModule(CodegenSession session, LoweringReport report)
    -> LoweringResult;

}  // namespace lyra::lowering::mir_to_llvm
