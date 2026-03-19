#include "lyra/llvm_backend/activation_local.hpp"

#include <algorithm>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/passes/activation_segment_analysis.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto IsSegmentExitBlock(const mir::BasicBlock& block) -> bool {
  return std::visit(
      [](const auto& t) -> bool {
        using T = std::decay_t<decltype(t)>;
        return std::is_same_v<T, mir::Delay> || std::is_same_v<T, mir::Wait> ||
               std::is_same_v<T, mir::Return> ||
               std::is_same_v<T, mir::Finish> || std::is_same_v<T, mir::Repeat>;
      },
      block.terminator.data);
}

auto TranslateBoundary(const mir::passes::BoundaryRecord& boundary)
    -> ContractBoundaryAction {
  switch (boundary.kind) {
    case mir::passes::BoundaryKind::kObservation:
      return ContractBoundaryAction{
          .block_index = boundary.block_index,
          .statement_index = boundary.statement_index,
          .action = SyncAction::kSyncManagedToCanonical,
          .reload_slots = {},
      };
    case mir::passes::BoundaryKind::kMayWriteAny:
      return ContractBoundaryAction{
          .block_index = boundary.block_index,
          .statement_index = boundary.statement_index,
          .action = SyncAction::kSyncManagedAndReloadAllManaged,
          .reload_slots = {},
      };
    case mir::passes::BoundaryKind::kWritebackSpecific:
      return ContractBoundaryAction{
          .block_index = boundary.block_index,
          .statement_index = boundary.statement_index,
          .action = SyncAction::kSyncManagedAndReloadSpecific,
          .reload_slots = boundary.affected_slots,
      };
  }
  throw common::InternalError("TranslateBoundary", "unhandled BoundaryKind");
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

    // Translate boundary records to contract boundary actions.
    std::vector<ContractBoundaryAction> boundary_actions;
    for (const auto& boundary : segment.boundaries) {
      boundary_actions.push_back(TranslateBoundary(boundary));
    }

    std::ranges::sort(boundary_actions, [](const auto& a, const auto& b) {
      if (a.block_index != b.block_index) return a.block_index < b.block_index;
      return a.statement_index < b.statement_index;
    });

    plan.segments.push_back(
        SegmentContract{
            .entry_block = segment.entry_block,
            .blocks = segment.blocks,
            .managed_slots = std::move(managed),
            .block_actions = std::move(block_actions),
            .boundary_actions = std::move(boundary_actions),
        });
  }

  return plan;
}

}  // namespace lyra::lowering::mir_to_llvm
