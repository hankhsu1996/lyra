#pragma once

#include <cstdint>

namespace lyra::runtime::process_meta_abi {

// ABI version. Bumped when field layout changes.
inline constexpr uint32_t kVersion = 1;

// Number of uint32_t words per process entry.
// Layout: [instance_path_str_off, kind_packed, file_str_off, line, col]
//   kind_packed = (kind) | (reserved0 << 8) | (reserved1 << 16)
inline constexpr uint32_t kStride = 5;

inline constexpr uint32_t kFieldInstancePathOff = 0;
inline constexpr uint32_t kFieldKindPacked = 1;
inline constexpr uint32_t kFieldFileStrOff = 2;
inline constexpr uint32_t kFieldLine = 3;
inline constexpr uint32_t kFieldCol = 4;

}  // namespace lyra::runtime::process_meta_abi
