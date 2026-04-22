#pragma once

#include <cstdint>

namespace lyra::runtime::trace_signal_meta_abi {

// Number of uint32_t words per trace signal entry.
// Layout: [name_str_off, bit_width, trace_kind, storage_owner_slot_id]
inline constexpr uint32_t kStride = 4;

inline constexpr uint32_t kFieldNameStrOff = 0;
inline constexpr uint32_t kFieldBitWidth = 1;
inline constexpr uint32_t kFieldTraceKind = 2;
// Canonical storage owner slot id. For storage owners, equals the slot's
// own id (entry index). For forwarded aliases, equals the canonical owner.
inline constexpr uint32_t kFieldStorageOwnerSlotId = 3;

}  // namespace lyra::runtime::trace_signal_meta_abi
