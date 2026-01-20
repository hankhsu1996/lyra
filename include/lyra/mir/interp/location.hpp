#pragma once

#include <cstdint>
#include <optional>

#include "lyra/common/type.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Computed bit slice information after evaluating BitRangeProjection offsets.
// Used to track accumulated bit-range state during projection application.
struct BitSlice {
  uint64_t
      total_offset;     // Sum of evaluated offsets (uint64 to detect overflow)
  uint32_t width;       // From innermost projection
  TypeId element_type;  // From innermost projection
};

// Location for read operations (const pointer to base).
struct ConstLocation {
  const RuntimeValue* base = nullptr;
  std::optional<BitSlice> bit_slice;
};

// Location for write operations (mutable pointer to base).
struct Location {
  RuntimeValue* base = nullptr;
  std::optional<BitSlice> bit_slice;
};

}  // namespace lyra::mir::interp
