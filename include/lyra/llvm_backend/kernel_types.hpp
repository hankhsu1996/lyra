#pragma once

#include <cstdint>
#include <optional>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/slot_id.hpp"
#include "lyra/metadata/design_metadata.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

// Pre-resolved trigger observation for sub-slot narrowing in metadata lowering.
// Computed during connection/comb kernel collection while the owning MIR arena
// is still known, so no arena-local PlaceId survives into cross-boundary
// metadata.
struct ResolvedObservation {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  uint8_t bit_index = 0;
};

// Entry for a connection process that has been kernelized.
// All kernelized connections come from port bindings. Collected before
// layout and consumed by BuildLayout as pre-collected input.
// Module-internal continuous assigns are lowered as always_comb processes
// and kernelized as CombKernels, not connections.
struct ConnectionKernelEntry {
  mir::ProcessId process_id;
  common::SlotId src_slot;
  common::SlotId dst_slot;
  common::SlotId trigger_slot;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  std::optional<ResolvedObservation> trigger_observation;
  metadata::ConnectionKernelOrigin origin =
      metadata::ConnectionKernelOrigin::kPortBinding;
};

}  // namespace lyra::lowering::mir_to_llvm
