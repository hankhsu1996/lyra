#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

struct ManagedSlot {
  mir::SignalRef slot;
  TypeId root_type;
};

struct ManagedSlotSet {
  std::vector<ManagedSlot> slots;
};

enum class SyncAction : uint8_t {
  kSyncManagedToCanonical,
  kSyncManagedAndReloadAllManaged,
  kSyncManagedAndReloadSpecific,
};

struct ContractBoundaryAction {
  uint32_t block_index;
  uint32_t statement_index;
  SyncAction action;
  std::vector<mir::SignalRef> reload_slots;
};

struct ContractBlockActions {
  bool seed_on_entry = false;
  bool sync_on_exit = false;
};

struct SegmentContract {
  uint32_t entry_block;
  std::vector<uint32_t> blocks;
  ManagedSlotSet managed_slots;

  // Keyed by real block index (not position in `blocks` vector).
  std::unordered_map<uint32_t, ContractBlockActions> block_actions;

  // Sorted by (block_index, statement_index).
  std::vector<ContractBoundaryAction> boundary_actions;
};

struct ProcessActivationPlan {
  std::vector<SegmentContract> segments;

  [[nodiscard]] auto HasActivationLocalSlots() const -> bool {
    return !segments.empty();
  }
};

auto BuildProcessActivationPlan(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types) -> ProcessActivationPlan;

}  // namespace lyra::lowering::mir_to_llvm
