#pragma once

#include <cstdint>
#include <span>
#include <unordered_set>
#include <vector>

#include "lyra/llvm_backend/forwarding_map.hpp"
#include "lyra/llvm_backend/kernel_types.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

// Pre-layout trigger slot summary for forwarding exclusion.
// Contains design-global slot IDs that are non-connection process/comb
// trigger targets, collected from MIR Wait terminators before layout.
//
// Connection processes are intentionally excluded: their trigger roles
// are fully represented in ConnectionKernelEntry and classified by
// ClassifyConnectionKernelSlotUses. This summary captures triggers from
// init processes and module body processes only. The two exclusion
// sources are disjoint by construction: connection processes live in
// design.connection_processes, not in module_plans.body_processes.
struct TriggerSlotSummary {
  // Design-global slot IDs that are trigger targets for non-connection
  // processes (init processes + module body processes: always_ff,
  // always_comb, etc.). Both process triggers and comb triggers are
  // included here; the first-cut predicate excludes both.
  std::unordered_set<uint32_t> non_connection_trigger_slots;
};

// Collect non-connection trigger slot sets from MIR processes before layout.
// Walks init processes and module body processes (NOT connection processes),
// extracts Wait terminator trigger signals, and canonicalizes module-local
// signals to design-global using design_state_base_slot from module_plans.
auto CollectTriggerSlotSummary(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena) -> TriggerSlotSummary;

// Record of a downstream src-use of a slot in a connection kernel.
struct DownstreamSrcUse {
  uint32_t conn_index = 0;
  // trigger_slot == this slot in this connection
  bool self_triggered = false;
  // trigger_observation is nullopt or byte_size == 0
  bool full_slot_trigger = false;
};

// Per-slot role classification across all connection kernels and trigger
// slot summaries. Built by BuildSlotUsageSummaries via the single
// classifier ClassifyConnectionKernelSlotUses.
struct SlotUsageSummary {
  // Connection indices where this slot is dst_slot (written by connection)
  std::vector<uint32_t> upstream_conn_indices;

  // Downstream src uses: connections where this slot is src_slot
  std::vector<DownstreamSrcUse> downstream_src_uses;

  // Connection indices where this slot is trigger_slot
  std::vector<uint32_t> trigger_conn_indices;

  // True if this slot appears as trigger_slot in a connection where it
  // is NOT the src_slot (trigger-only role, not part of a downstream
  // self-triggered read). Forbidden for relay candidates.
  bool has_non_downstream_trigger_role = false;

  // True if this slot appears in any ConnectionKernelEntry field not
  // classified as one of the three allowed roles (upstream dst, downstream
  // src, self-triggered trigger). Hard blocker for forwarding.
  bool has_any_other_connection_role = false;

  // Comb kernel trigger role (from TriggerSlotSummary)
  bool is_comb_trigger = false;

  // Process trigger role (from TriggerSlotSummary)
  bool is_process_trigger = false;
};

// Build per-slot usage summaries from connection kernels and trigger summary.
auto BuildSlotUsageSummaries(
    std::span<const ConnectionKernelEntry> connections,
    const TriggerSlotSummary& trigger_summary, uint32_t num_slots)
    -> std::vector<SlotUsageSummary>;

// First-cut candidate predicate for connection-relay forwarding.
// A slot passes this predicate if and only if:
//   - exactly one upstream connection writes to it (upstream_conn_indices == 1)
//   - at least one downstream src use exists
//   - every downstream src use is self-triggered with full-slot observation
//   - no non-downstream trigger-only roles
//   - no unclassified connection roles
//   - no comb trigger or process trigger roles
// Any unclassified role blocks forwarding by default.
auto IsCanonicalForwardableConnectionRelay(
    mir::SlotId slot_id, const SlotUsageSummary& summary) -> bool;

// Build the ForwardingMap from connection kernels and trigger summary.
// Only slots passing IsCanonicalForwardableConnectionRelay are included.
// Transitive chains are flattened with cycle detection.
auto BuildForwardingMap(
    std::span<const ConnectionKernelEntry> connections,
    const TriggerSlotSummary& trigger_summary, uint32_t num_slots)
    -> ForwardingMap;

// Resolve all slot fields in a connection kernel entry through the
// forwarding map. All other fields copy through unchanged.
auto CanonicalizeConnectionKernelEntry(
    const ConnectionKernelEntry& entry, const ForwardingMap& map)
    -> ConnectionKernelEntry;

// Check if two canonicalized connection kernel entries are semantically
// equivalent and can be merged. All semantic fields must match exactly:
// process_id, src_slot, dst_slot, trigger_slot, trigger_edge,
// trigger_observation, origin. No field is excluded from equality.
auto ConnectionKernelEntriesSemanticallyEquivalentForMerge(
    const ConnectionKernelEntry& a, const ConnectionKernelEntry& b) -> bool;

// Merge adjacent duplicate entries in a canonicalized connection list.
// Stable-sort by full semantic key, then merge adjacent entries where
// the semantic equivalence predicate is true.
auto MergeCanonicalConnectionKernelEntries(
    std::vector<ConnectionKernelEntry> entries)
    -> std::vector<ConnectionKernelEntry>;

// Rebuild the connection kernel list through canonical forwarding resolution.
// Pure rewrite: no semantic filtering (all decisions made in
// BuildForwardingMap). Resolves src_slot, dst_slot, trigger_slot through the
// map, drops identity connections (src == dst after resolution), and merges
// duplicates. Post-condition: no forwarded alias slot_id in any field of any
// output entry.
auto RebuildCanonicalConnections(
    std::vector<ConnectionKernelEntry> connections, const ForwardingMap& map)
    -> std::vector<ConnectionKernelEntry>;

// Verify all slot fields in canonicalized connections are resolved
// as intended: no forwarded alias in src_slot, dst_slot, or trigger_slot.
void VerifyCanonicalizedConnectionObservations(
    const ForwardingMap& map,
    std::span<const ConnectionKernelEntry> canonicalized);

}  // namespace lyra::lowering::mir_to_llvm
