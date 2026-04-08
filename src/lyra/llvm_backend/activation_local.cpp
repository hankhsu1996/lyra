#include "lyra/llvm_backend/activation_local.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/mir/passes/activation_segment_analysis.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto IsSegmentExitBlock(const mir::BasicBlock& block) -> bool {
  return std::visit(
      [](const auto& t) -> bool {
        using T = std::decay_t<decltype(t)>;
        return std::is_same_v<T, mir::Delay> || std::is_same_v<T, mir::Wait> ||
               std::is_same_v<T, mir::WaitEvent> ||
               std::is_same_v<T, mir::Return> ||
               std::is_same_v<T, mir::Finish> || std::is_same_v<T, mir::Repeat>;
      },
      block.terminator.data);
}

// Convert a semantic fact into an activation-local contract decision.
// This is the only place where sync/reload policy is decided.
//
// Rules:
//   conservative_unknown -> sync before + reload all after
//   reads_canonical_state -> sync before
//   writes_canonical_state + kSpecific -> reload specific after
//   writes_canonical_state + kAll -> reload all after
//   no canonical interaction -> no contract needed (nullopt)
//
// Pre-sync comes from canonical-read dependency or conservative
// fallback. Canonical writes imply post-reload requirements, not
// automatic pre-sync.
auto PlanStatementContract(const mir::passes::StatementSemanticFact& fact)
    -> std::optional<StatementContractPlan> {
  using WF = mir::passes::CanonicalWriteFootprint;

  bool sync_before = false;
  auto reload_after = ReloadScope::kNone;
  std::vector<mir::SignalRef> reload_targets;

  if (fact.conservative_unknown) {
    sync_before = true;
    reload_after = ReloadScope::kAll;
  } else {
    sync_before = fact.reads_canonical_state;

    if (fact.writes_canonical_state) {
      if (fact.write_footprint == WF::kSpecific) {
        reload_after = ReloadScope::kSpecific;
        reload_targets = fact.write_targets;
      } else if (fact.write_footprint == WF::kAll) {
        reload_after = ReloadScope::kAll;
      }
    }
  }

  if (!sync_before && reload_after == ReloadScope::kNone) {
    return std::nullopt;
  }

  return StatementContractPlan{
      .block_index = fact.block_index,
      .statement_index = fact.statement_index,
      .sync_before = sync_before,
      .reload_after = reload_after,
      .reload_targets = std::move(reload_targets),
  };
}

}  // namespace

auto BuildProcessActivationPlan(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types) -> ProcessActivationPlan {
  auto segments = mir::passes::AnalyzeActivationSegments(process, arena);

  ProcessActivationPlan plan;

  for (const auto& segment : segments) {
    auto seg_elig = mir::passes::EvaluateSegmentEligibility(segment);
    if (!seg_elig.eligible) continue;

    auto slot_eligibility =
        mir::passes::EvaluateSlotEligibility(segment, types);

    ManagedSlotSet managed;
    for (const auto& se : slot_eligibility) {
      if (se.eligible) {
        managed.slots.push_back(
            ManagedSlot{
                .slot = se.slot,
                .root_type = se.root_type,
            });
      }
    }
    if (managed.slots.empty()) continue;

    // Build block actions keyed by real block index.
    std::unordered_map<uint32_t, ContractBlockActions> block_actions;

    // Seed on entry at segment entry block.
    block_actions[segment.entry_block].seed_on_entry = true;

    // Sync on exit for blocks with segment-exit terminators.
    for (uint32_t bi : segment.blocks) {
      if (IsSegmentExitBlock(process.blocks[bi])) {
        block_actions[bi].sync_on_exit = true;
      }
    }

    // Convert semantic facts to contract plans. Only non-empty plans
    // (those requiring sync or reload) are stored.
    std::vector<StatementContractPlan> contracts;
    for (const auto& fact : segment.semantic_facts) {
      auto plan_entry = PlanStatementContract(fact);
      if (plan_entry) {
        contracts.push_back(std::move(*plan_entry));
      }
    }

    std::ranges::sort(contracts, [](const auto& a, const auto& b) {
      if (a.block_index != b.block_index) return a.block_index < b.block_index;
      return a.statement_index < b.statement_index;
    });

    plan.segments.push_back(
        SegmentContract{
            .entry_block = segment.entry_block,
            .blocks = segment.blocks,
            .managed_slots = std::move(managed),
            .block_actions = std::move(block_actions),
            .statement_contracts = std::move(contracts),
        });
  }

  return plan;
}

}  // namespace lyra::lowering::mir_to_llvm
