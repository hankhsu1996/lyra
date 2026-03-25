// Shared observable-descriptor extraction helpers.
// These helpers convert existing layout/type/MIR facts into the storage/trace
// classification values consumed by H5 descriptor extraction.
// They are intentionally pure classification/query helpers and should not
// depend on design-global metadata emission code.

#pragma once

#include <cstdint>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"

namespace lyra::mir {
enum class SlotKind : uint8_t;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

// Derive runtime slot storage kind from resolved storage spec.
auto ClassifySlotStorageKind(const SlotStorageSpec& spec)
    -> runtime::SlotStorageKind;

// Map MIR SlotKind to runtime TraceSignalKind.
auto MapSlotKindToTraceKind(mir::SlotKind kind) -> runtime::TraceSignalKind;

// Compute trace bit width from a type. Returns the packed bit width for
// integral/packed types, 0 for non-bit-vector types.
auto ComputeTraceBitWidth(TypeId type_id, const TypeArena& types) -> uint32_t;

}  // namespace lyra::lowering::mir_to_llvm
