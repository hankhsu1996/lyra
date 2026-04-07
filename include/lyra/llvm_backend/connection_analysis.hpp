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

// Connection analysis result for a design.
// Carries the original connection edges (using original slot IDs, no
// canonical-owner rewriting), per-slot usage summaries, and per-slot
// relay-candidate classification.
struct ConnectionAnalysisResult {
  std::vector<ConnectionKernelEntry> connection_edges;
  std::vector<SlotUsageSummary> slot_usage;
  std::vector<bool> is_relay_candidate;
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

}  // namespace lyra::lowering::mir_to_llvm
