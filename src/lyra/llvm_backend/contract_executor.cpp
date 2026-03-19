#include "lyra/llvm_backend/contract_executor.hpp"

#include <cstdint>
#include <optional>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

ContractExecutor::ContractExecutor(
    Context& ctx, const ProcessActivationPlan& plan,
    std::vector<ManagedSlotStorage> storage)
    : ctx_(ctx), plan_(plan), storage_(std::move(storage)), canonical_(ctx) {
  for (size_t si = 0; si < plan_.segments.size(); ++si) {
    for (uint32_t bi : plan_.segments[si].blocks) {
      block_to_segment_[bi] = si;
    }
  }
}

auto ContractExecutor::FindSegmentIndex(uint32_t block_index) const
    -> std::optional<size_t> {
  auto it = block_to_segment_.find(block_index);
  if (it == block_to_segment_.end()) return std::nullopt;
  return it->second;
}

auto ContractExecutor::FindBoundary(
    uint32_t block_index, uint32_t statement_index) const
    -> const ContractBoundaryAction* {
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return nullptr;
  const auto& seg = plan_.segments[*seg_idx];
  for (const auto& ba : seg.boundary_actions) {
    if (ba.block_index == block_index &&
        ba.statement_index == statement_index) {
      return &ba;
    }
  }
  return nullptr;
}

auto ContractExecutor::GetOrCreateResolver(size_t segment_index)
    -> ActivationLocalSlotAccess& {
  auto it = segment_resolvers_.find(segment_index);
  if (it != segment_resolvers_.end()) return it->second;

  const auto& seg = plan_.segments[segment_index];
  std::vector<ManagedSlotStorage> seg_storage;
  for (const auto& ms : storage_) {
    for (const auto& slot : seg.managed_slots.slots) {
      if (slot.slot == ms.slot) {
        seg_storage.push_back(ms);
        break;
      }
    }
  }
  auto [inserted, _] = segment_resolvers_.emplace(
      segment_index, ActivationLocalSlotAccess(ctx_, seg_storage));
  return inserted->second;
}

auto ContractExecutor::GetResolver(uint32_t block_index)
    -> SlotAccessResolver& {
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return canonical_;
  return GetOrCreateResolver(*seg_idx);
}

auto ContractExecutor::GetLifecycle(uint32_t block_index) -> SegmentLifecycle& {
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return canonical_;
  return GetOrCreateResolver(*seg_idx);
}

void ContractExecutor::ExecuteBlockEntry(uint32_t block_index) {
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return;
  const auto& seg = plan_.segments[*seg_idx];
  auto ba_it = seg.block_actions.find(block_index);
  if (ba_it != seg.block_actions.end() && ba_it->second.seed_on_entry) {
    GetOrCreateResolver(*seg_idx).SeedFromCanonical();
  }
}

void ContractExecutor::ExecutePreStatement(
    uint32_t block_index, uint32_t statement_index) {
  const auto* boundary = FindBoundary(block_index, statement_index);
  if (boundary == nullptr) return;
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return;
  // Pre-statement: sync managed slots to canonical so the boundary
  // statement sees up-to-date values.
  GetOrCreateResolver(*seg_idx).SyncToCanonical();
}

void ContractExecutor::ExecutePostStatement(
    uint32_t block_index, uint32_t statement_index) {
  const auto* boundary = FindBoundary(block_index, statement_index);
  if (boundary == nullptr) return;
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return;
  // Post-statement: reload managed slots from canonical if the
  // boundary statement may have modified them.
  switch (boundary->action) {
    case SyncAction::kSyncManagedToCanonical:
      break;
    case SyncAction::kSyncManagedAndReloadAllManaged:
      GetOrCreateResolver(*seg_idx).SeedFromCanonical();
      break;
    case SyncAction::kSyncManagedAndReloadSpecific:
      GetOrCreateResolver(*seg_idx).SyncAndReloadSpecific(
          boundary->reload_slots);
      break;
  }
}

void ContractExecutor::ExecuteBlockExit(uint32_t block_index) {
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return;
  const auto& seg = plan_.segments[*seg_idx];
  auto ba_it = seg.block_actions.find(block_index);
  if (ba_it != seg.block_actions.end() && ba_it->second.sync_on_exit) {
    GetOrCreateResolver(*seg_idx).SyncToCanonical();
  }
}

}  // namespace lyra::lowering::mir_to_llvm
