#pragma once

#include <cstdint>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/common/slot_id.hpp"
#include "lyra/llvm_backend/kernel_types.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

// Pipeline-owned pre-layout data: connection-kernel collection result.
// This is a pipeline-stage artifact, not a layout type. Built before layout
// and consumed by BuildLayout as pre-collected input.
struct ConnectionKernelCollectionResult {
  std::vector<ConnectionKernelEntry> kernel_entries;
  std::vector<mir::ProcessId> non_kernelized_processes;
};

// Narrowed slot-spec access for observation resolution.
// Only the fields needed by ResolveObservation: slot_to_index for lookup,
// slot_storage_specs for storage spec, storage_spec_arena for child specs.
// Forwarding-aware byte offsets and storage_owner_slot_id are NOT consumed.
struct SlotSpecView {
  const std::unordered_map<common::SlotId, uint32_t, SlotIdHash>& slot_to_index;
  const std::vector<SlotStorageSpec>& slot_storage_specs;
  const StorageSpecArena& storage_spec_arena;
};

// Collect connection kernels from connection processes.
// Trigger observation resolution uses SlotSpecView for byte-range
// computation from MIR PlaceId projections. Only storage specs and
// the spec arena are consumed; forwarding-aware byte offsets and
// storage_owner_slot_id are not needed.
auto CollectConnectionKernels(
    std::span<const mir::ProcessId> connection_processes,
    const mir::Arena& design_arena, const SlotSpecView& specs)
    -> ConnectionKernelCollectionResult;

}  // namespace lyra::lowering::mir_to_llvm
