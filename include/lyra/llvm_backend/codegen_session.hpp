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
#include "lyra/llvm_backend/deferred_thunk_abi.hpp"
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
// body is the canonical identity -- numeric body_id is derived when needed.
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

// Per-specialization slot access classification.
// After R2, every body-local slot owns storage. Access kind distinguishes
// inline value slots from owned-container slots.
enum class SpecSlotAccessKind : uint8_t {
  kOwnedInline,
  kOwnedContainer,
};

struct SpecSlotInfo {
  // Stable index into Layout::body_realization_infos for this body.
  static constexpr uint32_t kInvalidBodyInfoIndex = UINT32_MAX;
  uint32_t body_realization_info_index = kInvalidBodyInfoIndex;

  // Per-slot body-relative byte offset for local storage.
  // Every body-local slot has a valid offset (no forwarded aliases).
  // For kInlineValue: offset of the slot's value bytes from body base.
  // For kOwnedContainer: offset of the OwnedStorageHandle from body base.
  std::vector<BodyByteOffset> inline_offsets;
  // Per-slot storage shape.
  std::vector<mir::StorageShape> shapes;
  // Per-slot access classification.
  std::vector<SpecSlotAccessKind> access_kinds;

  // Access body-local slot storage spec through BodyRealizationInfo.
  // Single source of truth -- no duplication.
  [[nodiscard]] auto GetSlotSpec(uint32_t local_slot, const Layout& layout)
      const -> const SlotStorageSpec& {
    return layout.body_realization_infos.at(body_realization_info_index)
        .slot_specs.at(local_slot);
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

// Specialization compilation input: all data needed to compile one
// specialization body. Owns specialization-local backend data (MIR
// membership, layout, codegen routing). Does not reference orchestrator
// storage -- the specialization compiler reads only from this object.
// body is the canonical identity -- no numeric body_id.
struct CompiledModuleSpecInput {
  const mir::ModuleBody* body = nullptr;
  std::vector<mir::ProcessId> processes;
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
  // Specialization-local state. Owned by CompileDesignProcesses,
  // consumed by CompileModuleSpecSession via SpecLocalScope.
  const SpecSlotInfo* spec_slot_info = nullptr;
  const ConnectionNotificationMask* connection_notification_mask = nullptr;
};

// Process codegen product of compiling one specialization body.
// All callee captures are performed inside the session and returned here,
// so the caller never reads ambient Context state between sessions.
struct CompiledModuleSpec {
  const mir::ModuleBody* body = nullptr;
  // Parallel to input.view.processes: one compiled function per body process
  std::vector<llvm::Function*> process_functions;
  std::vector<WaitSiteEntry> wait_sites;
  // Parallel to process_functions: one optional trigger entry per body
  // process. Index by body process ordinal to get trigger facts.
  // scheduled_process_index is NOT yet set (stamped per-instance later).
  std::vector<std::optional<ProcessTriggerEntry>> process_triggers;
  // Per-body deferred assertion sites (forwarded from input).
  std::span<const mir::DeferredAssertionSiteInfo> deferred_sites;
  // Design-global base index (forwarded from input).
  uint32_t deferred_site_base_index = 0;
  // Deferred callee captures: one entry per body-local deferred site.
  // Captured inside the session while DeclaredFunctionScope is active.
  std::vector<DeferredSiteCalleeInfo> deferred_callee_info;
  // Body-local declared functions: (FunctionId, llvm::Function*).
  // Captured inside the session for DPI export wrapper resolution.
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_functions;
};

// Pure-data construction program: pooled paths, pooled param payloads, and
// a per-instance entry table in strict ModuleIndex order. Built by lowering,
// serialized to LLVM globals by emission, replayed by the runtime constructor.
struct ConstructionProgramData {
  std::vector<uint8_t> path_pool;
  std::vector<uint8_t> param_pool;
  std::vector<runtime::ConstructionProgramEntry> entries;
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
// parameter needed.
auto CompileModuleSpecSession(
    Context& context, const CompiledModuleSpecInput& input,
    const mir::ConstructionInput* construction) -> Result<CompiledModuleSpec>;

// Backend phase: extract LLVM ownership from a completed session.
auto FinalizeModule(CodegenSession session, LoweringReport report)
    -> LoweringResult;

}  // namespace lyra::lowering::mir_to_llvm
