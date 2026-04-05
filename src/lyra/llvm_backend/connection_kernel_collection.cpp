#include "lyra/llvm_backend/connection_kernel_collection.hpp"

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Pending connection kernel entry before observation resolution.
struct PendingConnectionKernelEntry {
  mir::ProcessId process_id;
  common::SlotId src_slot;
  common::SlotId dst_slot;
  common::SlotId trigger_slot;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  std::optional<mir::PlaceId> trigger_observed_place;
};

// Check if a connection process can be kernelized.
auto TryKernelizeConnection(
    const mir::Process& process, const mir::Arena& arena)
    -> std::optional<PendingConnectionKernelEntry> {
  if (process.blocks.size() != 1) return std::nullopt;
  const auto& block = process.blocks[0];
  if (block.statements.size() != 1) return std::nullopt;

  const auto* assign = std::get_if<mir::Assign>(&block.statements[0].data);
  if (assign == nullptr) return std::nullopt;

  auto dest_pid =
      mir::RequireLocalDest(assign->dest, "ConnectionKernelCollection");
  const auto& dest_place = arena[dest_pid];
  if (dest_place.root.kind != mir::PlaceRoot::Kind::kDesignGlobal)
    return std::nullopt;
  if (!dest_place.projections.empty()) return std::nullopt;

  const auto* rhs_operand = std::get_if<mir::Operand>(&assign->rhs);
  if (rhs_operand == nullptr) return std::nullopt;
  if (rhs_operand->kind != mir::Operand::Kind::kUse) return std::nullopt;

  auto src_place_id = std::get<mir::PlaceId>(rhs_operand->payload);
  const auto& src_place = arena[src_place_id];
  if (src_place.root.kind != mir::PlaceRoot::Kind::kDesignGlobal)
    return std::nullopt;
  if (!src_place.projections.empty()) return std::nullopt;

  const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
  if (wait == nullptr) return std::nullopt;
  if (wait->triggers.size() != 1) return std::nullopt;

  const auto& trigger = wait->triggers[0];
  if (trigger.late_bound.has_value()) return std::nullopt;

  if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
    throw common::InternalError(
        "TryKernelizeConnection", "connection trigger must be design-global");
  }

  return PendingConnectionKernelEntry{
      .process_id = {},
      .src_slot = common::SlotId{static_cast<uint32_t>(src_place.root.id)},
      .dst_slot = common::SlotId{static_cast<uint32_t>(dest_place.root.id)},
      .trigger_slot = common::SlotId{trigger.signal.id},
      .trigger_edge = trigger.edge,
      .trigger_observed_place = trigger.observed_place,
  };
}

}  // namespace

auto CollectConnectionKernels(
    std::span<const mir::ProcessId> connection_processes,
    const mir::Arena& design_arena, const SlotSpecView& specs)
    -> ConnectionKernelCollectionResult {
  ConnectionKernelCollectionResult result;

  // Build a temporary DesignLayout-like view for ResolveObservation.
  // ResolveObservation is a layout-domain public API (declared in layout.hpp).
  // We construct a minimal DesignLayout with only the fields it needs.
  DesignLayout temp_layout;
  temp_layout.slot_to_index = specs.slot_to_index;
  temp_layout.slot_storage_specs = specs.slot_storage_specs;
  temp_layout.storage_spec_arena = specs.storage_spec_arena;

  for (mir::ProcessId proc_id : connection_processes) {
    const auto& process = design_arena[proc_id];
    auto pending = TryKernelizeConnection(process, design_arena);
    if (pending) {
      std::optional<ResolvedObservation> obs;
      if (pending->trigger_observed_place) {
        obs = ResolveObservation(
            design_arena, temp_layout, pending->trigger_slot,
            *pending->trigger_observed_place);
      }
      result.kernel_entries.push_back({
          .process_id = proc_id,
          .src_slot = pending->src_slot,
          .dst_slot = pending->dst_slot,
          .trigger_slot = pending->trigger_slot,
          .trigger_edge = pending->trigger_edge,
          .trigger_observation = obs,
      });
    } else {
      result.non_kernelized_processes.push_back(proc_id);
    }
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
