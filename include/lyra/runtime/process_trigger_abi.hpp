#pragma once

#include <cstdint>

namespace lyra::runtime::process_trigger_abi {

// Process trigger word table row layout constants.
// Shared between serialization (realization) and runtime parsing.
// Format: [num_entries, per-row fields...]
inline constexpr uint32_t kStride = 4;
inline constexpr uint32_t kFieldProcessIndex = 0;
inline constexpr uint32_t kFieldSlotId = 1;
inline constexpr uint32_t kFieldEdgeKind = 2;
inline constexpr uint32_t kFieldFlags = 3;
inline constexpr uint32_t kFlagGroupable = 1U << 0;
// slot_id is body-local (not yet relocated to dense coordination space).
// Engine init applies observability.flat_coord_base + slot_id.
// When clear, slot_id is already in dense coordination space (global/absolute).
inline constexpr uint32_t kFlagBodyLocal = 1U << 1;

}  // namespace lyra::runtime::process_trigger_abi

namespace lyra::runtime {

// Per-trigger flag in the comb kernel word table.
// slot_id is body-local (not yet relocated to dense coordination space).
// Engine init applies observability.flat_coord_base + slot_id.
inline constexpr uint32_t kCombTriggerFlagBodyLocal = 1U << 0;

}  // namespace lyra::runtime
