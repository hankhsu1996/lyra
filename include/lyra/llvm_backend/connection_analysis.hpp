#pragma once

#include <cstdint>
#include <span>
#include <unordered_set>
#include <vector>

#include "lyra/llvm_backend/kernel_types.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {

// Pre-layout trigger slot summary for connection analysis.
// Contains design-global slot IDs that are non-connection process/comb
// trigger targets, collected from MIR Wait terminators before layout.
//
// Connection processes are intentionally excluded: their trigger roles
// are fully represented in ConnectionKernelEntry and classified by
// ClassifyConnectionKernelSlotUses. This summary captures triggers from
// init processes and module body processes only.
struct TriggerSlotSummary {
  std::unordered_set<uint32_t> non_connection_trigger_slots;
  // Slots whose values are read by non-connection process bodies
  // (e.g., $display reading a relay wire). A relay candidate with
  // process-body readers cannot be eliminated because the relay slot's
  // storage must remain updated for those reads to see correct values.
  std::unordered_set<uint32_t> process_body_read_slots;
};

// Collect non-connection trigger slot sets from MIR processes before layout.
auto CollectTriggerSlotSummary(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena) -> TriggerSlotSummary;

// Record of a downstream src-use of a slot in a connection kernel.
struct DownstreamSrcUse {
  uint32_t conn_index = 0;
  bool self_triggered = false;
  bool full_slot_trigger = false;
};

// Per-slot role classification across all connection kernels and trigger
// slot summaries.
struct SlotUsageSummary {
  std::vector<uint32_t> upstream_conn_indices;
  std::vector<DownstreamSrcUse> downstream_src_uses;
  std::vector<uint32_t> trigger_conn_indices;
  bool has_non_downstream_trigger_role = false;
  bool is_non_connection_trigger = false;
  bool is_process_body_read = false;
  bool is_container = false;
};

// Build per-slot usage summaries from connection kernels and trigger summary.
auto BuildSlotUsageSummaries(
    std::span<const ConnectionKernelEntry> connections,
    const TriggerSlotSummary& trigger_summary, uint32_t num_slots)
    -> std::vector<SlotUsageSummary>;

// Trivial-relay predicate for connection routing analysis.
// Returns true if a slot is a trivial relay candidate: exactly one upstream
// writer, all downstream uses are self-triggered full-slot observation,
// no process/comb triggers, no non-downstream trigger roles.
//
// true does NOT mean "remove local storage". Every slot owns storage.
// true means "this slot may be bypassable in realized routing/fanout".
auto IsTrivialRelayCandidate(const SlotUsageSummary& summary) -> bool;

// Identity-copy comb: a module body process that just copies one slot
// to another (e.g., `assign data_out = data_reg`). When the output slot
// is a relay candidate, downstream connections are rewritten to read
// from the source slot directly. The comb itself continues to execute
// inline (cheaper than full process activation) but its output becomes
// dead storage -- no downstream connections or subscriptions observe it.
struct IdentityCopyComb {
  common::SlotId src_slot;
  common::SlotId dst_slot;
  common::SlotId trigger_slot;
};

// Connection analysis result for a design.
// Carries the original connection edges (using original slot IDs, no
// canonical-owner rewriting), per-slot usage summaries, and per-slot
// relay-candidate classification.
struct ConnectionAnalysisResult {
  std::vector<ConnectionKernelEntry> connection_edges;
  std::vector<SlotUsageSummary> slot_usage;
  std::vector<bool> is_relay_candidate;
  // Identity-copy combs detected during analysis. Consumed by
  // EliminateRelayConnections to extend relay elimination to
  // comb-backed pass-throughs.
  std::vector<IdentityCopyComb> identity_copy_combs;
};

// Analyze connections for a design. Produces connection edges using original
// slot IDs (no forwarding canonicalization) and per-slot relay-candidate
// classification for future routing optimization.
// expanded_num_slots: total design-global slot count (package + expanded
// module instances). Temporary adapter for B2a transition where design.slots
// is package-only but connection kernel entries use full-range slot IDs.
// Pass 0 to use design.slots.size() (pre-B2a behavior).
auto AnalyzeConnections(
    std::vector<ConnectionKernelEntry> kernel_entries,
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, uint32_t expanded_num_slots = 0)
    -> ConnectionAnalysisResult;

// Compile-time connection elimination for transform-safe relay candidates.
// For each relay slot R with upstream edge U->R and downstream edges R->D...,
// rewrites downstream edges to read directly from U (U->D...) and deletes the
// upstream edge. Does not change storage layout or slot ownership.
//
// Safety boundary (first cut):
//   - exactly 1 upstream edge (port-binding origin, full-slot copy)
//   - all downstream edges are port-binding, self-triggered, full-slot
//   - no process/comb triggers on the relay slot
//
// Returns the number of relay slots eliminated.
auto EliminateRelayConnections(ConnectionAnalysisResult& analysis) -> uint32_t;

}  // namespace lyra::lowering::mir_to_llvm
