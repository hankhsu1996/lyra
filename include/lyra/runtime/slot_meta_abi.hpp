#pragma once

#include <cstdint>

namespace lyra::runtime::slot_meta_abi {

// ABI version. Bumped when field layout changes.
// Codegen passes this to LyraRunSimulation; runtime validates before parsing.
// v2: Split kHandle(2) into kString(2) + kHandle(3), renumber kAggregate to 4.
inline constexpr uint32_t kVersion = 2;

// Number of uint32_t words per row.
inline constexpr uint32_t kStride = 7;

// Field indices within a row (each row is kStride consecutive uint32_t).
inline constexpr uint32_t kFieldBaseOff = 0;
inline constexpr uint32_t kFieldTotalBytes = 1;
inline constexpr uint32_t kFieldKind = 2;
inline constexpr uint32_t kFieldValueOff = 3;
inline constexpr uint32_t kFieldValueBytes = 4;
inline constexpr uint32_t kFieldUnkOff = 5;
inline constexpr uint32_t kFieldUnkBytes = 6;

}  // namespace lyra::runtime::slot_meta_abi
