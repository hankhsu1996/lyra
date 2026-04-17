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
#include "lyra/common/ext_ref_binding.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/deferred_thunk_abi.hpp"
#include "lyra/llvm_backend/dpi_abi.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lowering_reports.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/runtime/construction_program_abi.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct LoweringInput;
struct LoweringResult;

// Per-instance binding data for specialization compilation.
// Stores stable IDs/values; variant is looked up from Layout when needed.
struct SpecInstanceBinding {
  ModuleIndex module_index;
};

// Specialization-owned MIR content: identity + behavioral IR references.
// Does not contain backend layout or codegen routing data.
// body pointer is the canonical identity.
struct SpecCompilationUnit {
  const mir::ModuleBody* body = nullptr;
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
  std::string func_name;  // body_{idx}_proc_{nonfinal_proc_ordinal}
};

// Narrow backend view for compiling one specialization body.
// Contains body-owned process routing data.
// Ephemeral: built and consumed inside CompileDesignProcesses, not persisted.
struct SpecCodegenView {
  std::vector<SpecProcessView> processes;
};

// CU-local layout contract: the subset of Layout data that one
// specialization body needs for compilation. Extracted from the
// design-global Layout during spec planning so CU compilation
// does not index into design-global layout arrays.
struct SpecLayoutContract {
  // Per-process ProcessLayout pointers, parallel to SpecCodegenView.processes.
  // Points into the design-global Layout::processes array (stable lifetime).
  std::vector<const ProcessLayout*> process_layouts;
  // Body-local slot storage specs, indexed by body-local slot id.
  // Copied from BodyRealizationInfo::slot_specs.
  std::vector<SlotStorageSpec> slot_specs;
  // Body-local behavioral dirty-propagation contract.
  // Copied from BodyRealizationInfo::slot_has_behavioral_trigger.
  std::vector<bool> slot_has_behavioral_trigger;
  // Per body-local slot: true iff a process in a different body (or an
  // init process) has a behavioral trigger that resolves to this slot,
  // AND this body does not already have a body-local trigger on it.
  // Disjoint with slot_has_behavioral_trigger by construction.
  // Copied from BodyRealizationInfo::slot_has_cross_body_behavioral_trigger.
  std::vector<bool> slot_has_cross_body_behavioral_trigger;
};

// Per-specialization slot access classification.
// After R2, every body-local slot owns storage. Access kind distinguishes
// inline value slots from owned-container slots.
enum class SpecSlotAccessKind : uint8_t {
  kOwnedInline,
  kOwnedContainer,
};

struct SpecSlotInfo {
  // Per-slot body-relative byte offset for local storage.
  // Every body-local slot has a valid offset (no forwarded aliases).
  // For kInlineValue: offset of the slot's value bytes from body base.
  // For kOwnedContainer: offset of the OwnedStorageHandle from body base.
  std::vector<BodyByteOffset> inline_offsets;
  // Per-slot appendix byte offset (for kOwnedContainer backing data).
  // nullopt for non-container slots.
  std::vector<std::optional<BodyByteOffset>> appendix_offsets;
  // Per-slot storage shape.
  std::vector<mir::StorageShape> shapes;
  // Per-slot access classification.
  std::vector<SpecSlotAccessKind> access_kinds;

  // Access body-local slot storage spec through the CU layout contract.
  [[nodiscard]] static auto GetSlotSpec(
      uint32_t local_slot, const SpecLayoutContract& contract)
      -> const SlotStorageSpec& {
    return contract.slot_specs.at(local_slot);
  }

  [[nodiscard]] auto SlotCount() const -> uint32_t {
    return static_cast<uint32_t>(shapes.size());
  }
  // Unchecked predicate. Callers must validate id < SlotCount() first.
  [[nodiscard]] auto IsOwnedContainer(uint32_t id) const -> bool {
    return shapes[id] == mir::StorageShape::kOwnedContainer;
  }
};

// Topology-derived codegen compilation decision: per-body connection
// notification mask. Separate from SpecSlotInfo because this is NOT a
// body semantic fact -- it depends on design topology (which instances
// have connection processes triggering on which slots).
// Indexed parallel to spec compilation units / spec_slot_infos.
struct ConnectionNotificationMask {
  // Per body-local slot: true iff any instance of this body has a
  // connection process that triggers on this slot. Conservative union.
  std::vector<bool> required;

  [[nodiscard]] auto IsRequired(uint32_t local_slot) const -> bool {
    if (local_slot >= required.size()) {
      throw common::InternalError(
          "ConnectionNotificationMask::IsRequired",
          std::format(
              "local_slot {} out of range (size={})", local_slot,
              required.size()));
    }
    return required[local_slot];
  }
};

// Pre-resolved module-scoped DPI export target for one body.
// Precomputed during planning so the session does not scan the
// design-global wrapper list.
struct ModuleExportTarget {
  uint32_t wrapper_index = 0;
  mir::FunctionId function_id;
};

// Per-wrapper resolved module export callee, produced by the body session.
// wrapper_index identifies the position in the design-global wrapper list.
struct ResolvedModuleExportEntry {
  uint32_t wrapper_index = 0;
  dpi::ModuleExportCalleeInfo info;
};

// Specialization compilation input: all data needed to compile one
// specialization body. Owns specialization-local backend data (MIR
// membership, layout, codegen routing). Does not reference orchestrator
// storage -- the specialization compiler reads only from this object.
// body pointer is the canonical identity.
struct CompiledModuleSpecInput {
  const mir::ModuleBody* body = nullptr;
  std::vector<mir::FunctionId> functions;
  SpecCodegenView view;
  // LLVM symbol naming prefix, precomputed from body index.
  // E.g., "body_3" for the fourth specialization body.
  std::string name_prefix;
  // Origin provenance for this body's diagnostic resolution.
  // Null when origin provenance is unavailable.
  const lowering::BodyOriginProvenance::Entry* origin_entry = nullptr;
  // Per-body deferred assertion sites (from body-owned storage).
  std::span<const mir::DeferredAssertionSiteInfo> deferred_sites;
  // Base indices for body-local-to-global remapping. MIR effects carry
  // body-local site IDs; the backend adds the base to produce design-global
  // runtime indices.
  uint32_t deferred_site_base_index = 0;
  uint32_t cover_site_base_index = 0;
  uint32_t wait_site_base_index = 0;
  uint32_t back_edge_site_base_index = 0;
  // Module-scoped DPI export targets for this body, precomputed by planning.
  // Each entry carries the design-global wrapper index and the body-local
  // function to resolve. Empty when no module-scoped DPI exports target
  // this body.
  std::span<const ModuleExportTarget> module_export_targets;
  // Specialization-local state. Owned by CompileDesignProcesses,
  // consumed by CompileModuleSpecSession via SpecLocalScope.
  const SpecSlotInfo* spec_slot_info = nullptr;
  const ConnectionNotificationMask* connection_notification_mask = nullptr;
  // CU-local layout contract: per-process layouts, body-local slot specs,
  // and behavioral trigger mask. Built from the design-global Layout during
  // spec planning. CU compilation reads layout data from this contract
  // instead of indexing into the design-global Layout.
  const SpecLayoutContract* layout_contract = nullptr;
};

// Process codegen product of compiling one specialization body.
// All compilation (processes, thunks) is performed inside the session.
// The caller never reads ambient Context state between sessions.
struct CompiledModuleSpec {
  const mir::ModuleBody* body = nullptr;
  // Parallel to input.view.processes: one compiled function per body process
  std::vector<llvm::Function*> process_functions;
  std::vector<WaitSiteEntry> wait_sites;
  // Parallel to process_functions: one optional trigger entry per body
  // process. Index by body process ordinal to get trigger facts.
  // scheduled_process_index is NOT yet set (stamped per-instance later).
  std::vector<std::optional<ProcessTriggerEntry>> process_triggers;
  // Design-global base index for deferred site concatenation.
  uint32_t deferred_site_base_index = 0;
  // Per-body compiled deferred assertion thunk artifacts.
  // Parallel to body's deferred_assertion_sites. Compiled inside the
  // session while declared functions are in scope.
  std::vector<DeferredSiteCompiledArtifact> deferred_artifacts;
  // Resolved module-scoped DPI export callees for this body.
  // Each entry carries the design-global wrapper index and resolved callee.
  // Captured inside the session while declared functions and MIR ABI
  // contracts are in scope. Spliced positionally by merge.
  std::vector<ResolvedModuleExportEntry> module_export_entries;
  // Per-body back-edge site origins, accumulated during CU codegen.
  // Spliced positionally into the design-global back-edge origin array.
  std::vector<common::OriginId> back_edge_origins;
};

// Pure-data construction program: pooled paths, pooled param payloads, and
// a per-instance entry table in strict ModuleIndex order. Built by lowering,
// serialized to LLVM globals by emission, replayed by the runtime constructor.
struct ConstructionProgramData {
  std::vector<uint8_t> path_pool;
  std::vector<uint8_t> param_pool;
  std::vector<runtime::ConstructionProgramEntry> entries;
  // Per-instance ext-ref binding records, packed flat.
  // ext_ref_binding_offsets[i] is the index into binding_pool for instance i
  // (UINT32_MAX if that instance has no external refs).
  // ext_ref_binding_counts[i] is the number of bindings for instance i.
  std::vector<common::SerializedExtRefBinding> ext_ref_binding_pool;
  std::vector<uint32_t> ext_ref_binding_offsets;
  std::vector<uint32_t> ext_ref_binding_counts;
  // LLVM emission helper only. Per-body mapping from body-local
  // child_site_index (ChildBindingSiteId) to tree-relative
  // ordinal_in_parent in the runtime scope tree. Produced and consumed
  // strictly during descriptor emission to bake the tree-relative
  // ExprConnChildDesc::child_ordinal into emitted LLVM globals. It must
  // not appear in any runtime ABI struct, C ABI boundary, or constructor
  // replay consumer. If a runtime-side component needs this mapping,
  // the design is wrong and the child-site concept should be replaced
  // at its source, not re-exposed through a second bridge.
  // Indexed by body_group; each inner vector is parallel to child_sites.
  std::vector<std::vector<uint32_t>> child_site_to_tree_ordinal;
  // Constant port connection initialization records + value pool.
  // Applied inline during child creation in LyraConstructorRunProgram.
  std::vector<runtime::PortConstInitEntry> port_const_inits;
  std::vector<uint8_t> port_const_pool;
};

// Design-derived inputs for the realization/assembly phase, extracted during
// CompileDesignProcesses. Carries the pure-data construction program built
// by lowering, serialized to LLVM globals by emission, and replayed by the
// runtime constructor. Entries are in strict ModuleIndex order. Runtime
// relies on this order for instance_id and flat slot-base allocation.
struct RealizationData {
  ConstructionProgramData construction_program;
};

// Backend-owned intermediate state between behavioral codegen and assembly.
// This is a strict bridge object: assembly may append IR (main()) to the
// module owned by context, but must not own layout/process compilation
// decisions. Assembly owns two products: realization descriptors
// (body/connection/schema globals consumed by the runtime constructor)
// and the runtime entry (main function, ABI setup, simulation dispatch).
//
// Layout and Context are heap-allocated because Context stores
// `const Layout&` internally. Returning CodegenSession from
// CompileDesignProcesses via std::expected may move the struct, so both
// must have stable addresses to keep the reference valid.
struct CodegenSession {
  std::unique_ptr<Layout> layout;
  std::unique_ptr<CuFacts> facts;
  std::unique_ptr<Context> context;
  RealizationData realization;
  std::vector<llvm::Function*> process_funcs;
  std::vector<WaitSiteEntry> wait_sites;
  size_t num_init_processes = 0;
  // Per-body compiled process functions for body
  // realization descriptor emission. Parallel to
  // layout->body_realization_infos: entry [i] corresponds to
  // body_realization_infos[i]. Each inner vector is parallel to the
  // body's non-final process list.
  struct BodyCompiledFuncs {
    std::vector<llvm::Function*> functions;
  };
  std::vector<BodyCompiledFuncs> body_compiled_funcs;

  // Per-site compiled deferred assertion thunk artifacts.
  // Positional: element [i] corresponds to deferred_assertion_sites[i].
  // Produced by CompileDeferredAssertionArtifacts, consumed by metadata
  // emission. Single-pipeline compilation product.
  std::vector<DeferredSiteCompiledArtifact> deferred_site_artifacts;
  // Design-global back-edge site origins for metadata emission.
  // Concatenated from spec products + standalone products.
  std::vector<common::OriginId> back_edge_origins;
};

// Backend phase: compile all design processes into LLVM IR.
// Produces a CodegenSession that assembly can append main() into.
auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession>;

// Compile one specialization body: the single native per-specialization backend
// entrypoint. Registers monitors, declares/defines body-local functions,
// generates shared/template process functions, and returns an explicit product.
// Does not inspect design-global state, package functions, or wrapper logic.
// All body-specific data is carried on the input -- no design or provenance
// parameter needed. Per-instance data (external ref slots) is loaded at
// runtime from instance_ptr, not passed through the compilation session.
auto CompileModuleSpecSession(
    Context& context, const CompiledModuleSpecInput& input)
    -> Result<CompiledModuleSpec>;

// Backend phase: extract LLVM ownership from a completed session.
auto FinalizeModule(CodegenSession session, LoweringReport report)
    -> LoweringResult;

}  // namespace lyra::lowering::mir_to_llvm
