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

enum class ReloadScope : uint8_t {
  kNone,
  kSpecific,
  kAll,
};

// Activation-local contract decision for one statement.
// Produced by contract planning from semantic facts.
// Consumed mechanically by the executor.
struct StatementContractPlan {
  uint32_t block_index;
  uint32_t statement_index;

  bool sync_before;
  ReloadScope reload_after;
  std::vector<mir::SignalRef> reload_targets;
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
  std::vector<StatementContractPlan> statement_contracts;
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
