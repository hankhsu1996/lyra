#pragma once

#include <cstdint>

namespace lyra::runtime::trace_signal_meta_abi {

// Number of uint32_t words per trace signal entry.
// Layout: [name_str_off, bit_width, trace_kind]
inline constexpr uint32_t kStride = 3;

inline constexpr uint32_t kFieldNameStrOff = 0;
inline constexpr uint32_t kFieldBitWidth = 1;
inline constexpr uint32_t kFieldTraceKind = 2;

}  // namespace lyra::runtime::trace_signal_meta_abi
