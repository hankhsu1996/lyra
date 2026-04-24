#include "lyra/llvm_backend/contract_executor.hpp"

#include <cstdint>
#include <optional>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

ContractExecutor::ContractExecutor(
    Context& ctx, const CuFacts& facts, const ProcessActivationPlan& plan,
    std::vector<ManagedSlotStorage> storage)
    : ctx_(ctx),
      facts_(facts),
      plan_(plan),
      storage_(std::move(storage)),
      canonical_(ctx, facts) {
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

auto ContractExecutor::FindStatementContract(
    uint32_t block_index, uint32_t statement_index) const
    -> const StatementContractPlan* {
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return nullptr;
  const auto& seg = plan_.segments[*seg_idx];
  for (const auto& sc : seg.statement_contracts) {
    if (sc.block_index == block_index &&
        sc.statement_index == statement_index) {
      return &sc;
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
      segment_index, ActivationLocalSlotAccess(ctx_, facts_, seg_storage));
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
  const auto* contract = FindStatementContract(block_index, statement_index);
  if (contract == nullptr) return;
  if (!contract->sync_before) return;
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return;
  GetOrCreateResolver(*seg_idx).SyncToCanonical();
}

void ContractExecutor::ExecutePostStatement(
    uint32_t block_index, uint32_t statement_index) {
  const auto* contract = FindStatementContract(block_index, statement_index);
  if (contract == nullptr) return;
  auto seg_idx = FindSegmentIndex(block_index);
  if (!seg_idx) return;
  switch (contract->reload_after) {
    case ReloadScope::kNone:
      break;
    case ReloadScope::kSpecific:
      GetOrCreateResolver(*seg_idx).SyncAndReloadSpecific(
          contract->reload_targets);
      break;
    case ReloadScope::kAll:
      GetOrCreateResolver(*seg_idx).SeedFromCanonical();
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
