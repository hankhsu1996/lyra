#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/realization/design_metadata.hpp"

namespace lyra::mir {
class Arena;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

class Context;
struct Layout;
struct DesignLayout;
struct SlotInfo;

// Extract slot metadata inputs from LLVM layout into plain link structs.
auto ExtractSlotMetaInputs(
    const std::vector<SlotInfo>& slots, const DesignLayout& design_layout)
    -> std::vector<realization::SlotMetaInput>;

// Extract scheduled process inputs into plain link structs.
// Resolves each process from its owning arena: design arena for
// standalone processes, body arena for module-bound processes.
auto PrepareScheduledProcessInputs(
    const std::vector<std::string>& instance_paths, const mir::Design& design,
    const mir::Arena& design_arena, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager,
    const std::vector<struct ScheduledProcess>& scheduled_processes,
    size_t num_init) -> std::vector<realization::ScheduledProcessInput>;

// Extract back-edge site inputs from accumulated codegen origins.
auto PrepareBackEdgeSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<realization::BackEdgeSiteInput>;

// Extract connection descriptor entries from canonical layout.
// Trigger observation data is pre-resolved in the layout; no arena access
// needed.
auto ExtractConnectionDescriptorEntries(const Layout& layout)
    -> std::vector<realization::ConnectionDescriptorEntry>;

// Prepare comb kernel inputs from layout data.
// Trigger observation data is pre-resolved in the layout; no arena access
// needed. Canonicalizes to one trigger per (kernel, slot).
auto PrepareCombKernelInputs(const Layout& layout, size_t num_init)
    -> std::vector<realization::CombKernelInput>;

// Build constructor-visible process trigger inputs from canonical entries.
// Validates the design-global signal invariant, classifies Stage-1
// groupability (static shape, uniform edge, full-slot), and flattens
// to one ProcessTriggerInput row per trigger fact.
//
// Precondition: all signal refs in entries must be design-global.
// Module-local signals must have been canonicalized by the caller
// (lower.cpp per-instance trigger collection).
auto BuildProcessTriggerInputs(
    const std::vector<ProcessTriggerEntry>& entries, uint32_t slot_count)
    -> std::vector<realization::ProcessTriggerInput>;

// Build final trace-signal metadata inputs from compile-owned provenance.
// Assembles hierarchical names, computes bit widths, maps trace kinds.
// Each slot's provenance carries its scope kind and scope ref for O(1) lookup.
auto PrepareTraceSignalMetaInputs(
    const std::vector<mir::SlotTraceProvenance>& provenance,
    const std::vector<char>& trace_string_pool,
    const std::vector<TypeId>& slot_types,
    const std::vector<mir::SlotKind>& slot_kinds,
    const std::vector<std::string>& instance_paths, const TypeArena& types)
    -> std::vector<realization::TraceSignalMetaInput>;

// Port-binding forwarding candidate. Analysis-only result; does NOT
// authorize transformation. Each field records the result of one
// candidate check for reviewability. Unresolved checks are marked
// explicitly.
struct PortBindingForwardingCandidate {
  uint32_t intermediate_slot_id = 0;
  uint32_t upstream_connection_index = 0;
  uint32_t downstream_connection_index = 0;

  // Provably checked conditions.
  bool single_writer = false;
  bool single_downstream = false;
  bool both_port_binding = false;
  bool no_process_trigger = false;
  bool no_comb_trigger = false;

  // Shape checks -- necessary but not sufficient for full proof.
  // upstream_full_copy_shape: upstream uses full-slot trigger and copies
  // nonzero bytes. Does NOT prove the copy covers the entire slot extent.
  // downstream_matching_read_shape: downstream reads from the same byte
  // region that upstream writes. Does NOT prove semantic value equivalence.
  bool upstream_full_copy_shape = false;
  bool downstream_matching_read_shape = false;

  // Unresolved: active trace/display/strobe references cannot be
  // determined from compile-time metadata alone.
  // NOT checked in the candidate filter.
  bool trace_ref_unresolved = true;
};

// Find port-binding forwarding candidates. Analysis only -- does not
// modify the connection graph or prove transform safety. Emits all
// candidates that pass the current provable checks. Unresolved checks
// (like active trace references) are flagged but do not exclude.
auto FindPortBindingForwardingCandidates(
    std::span<const realization::ConnectionDescriptorEntry> connections,
    std::span<const realization::ProcessTriggerInput> process_triggers,
    std::span<const realization::CombKernelInput> comb_inputs,
    uint32_t num_slots) -> std::vector<PortBindingForwardingCandidate>;

// Log forwarding candidate analysis results to stderr.
void LogPortBindingForwardingCandidates(
    std::span<const PortBindingForwardingCandidate> candidates);

// Result of emitting DesignMetadata as LLVM globals.
struct MetadataGlobals {
  llvm::Constant* slot_meta_words = nullptr;
  uint32_t slot_meta_count = 0;
  llvm::Constant* process_meta_words = nullptr;
  uint32_t process_meta_count = 0;
  llvm::Constant* process_meta_pool = nullptr;
  uint32_t process_meta_pool_size = 0;
  llvm::Constant* back_edge_site_meta_words = nullptr;
  uint32_t back_edge_site_meta_count = 0;
  llvm::Constant* back_edge_site_meta_pool = nullptr;
  uint32_t back_edge_site_meta_pool_size = 0;
  llvm::Value* conn_desc_table = nullptr;
  uint32_t conn_desc_count = 0;
  llvm::Value* comb_kernel_words = nullptr;
  uint32_t comb_kernel_word_count = 0;
  llvm::Value* process_trigger_words = nullptr;
  uint32_t process_trigger_word_count = 0;
  llvm::Value* instance_paths_array = nullptr;
  uint32_t instance_path_count = 0;
  llvm::Constant* trace_signal_meta_words = nullptr;
  uint32_t trace_signal_meta_word_count = 0;
  llvm::Constant* trace_signal_meta_pool = nullptr;
  uint32_t trace_signal_meta_pool_size = 0;
};

// Emit DesignMetadata as LLVM globals. Pure emission only: no metadata
// table packing or serialization. All tables in DesignMetadata are already
// runtime-shaped.
auto EmitDesignMetadataGlobals(
    Context& context, const realization::DesignMetadata& metadata,
    llvm::IRBuilder<>& builder) -> MetadataGlobals;

}  // namespace lyra::lowering::mir_to_llvm
