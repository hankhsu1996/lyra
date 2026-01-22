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

// Track union member view for proper codec dispatch (const version for reads).
struct ConstUnionView {
  const RuntimeUnion* union_ptr;
  uint32_t member_index;
  TypeId union_type;  // To retrieve UnpackedUnionInfo for codec
};

// Track union member view for proper codec dispatch (mutable version for
// writes).
struct UnionView {
  RuntimeUnion* union_ptr;
  uint32_t member_index;
  TypeId union_type;  // To retrieve UnpackedUnionInfo for codec
};

// Location for read operations (const pointer to base).
struct ConstLocation {
  const RuntimeValue* base = nullptr;
  std::optional<BitSlice> bit_slice;
  std::optional<ConstUnionView> union_view;
};

// Location for write operations (mutable pointer to base).
struct Location {
  RuntimeValue* base = nullptr;
  std::optional<BitSlice> bit_slice;
  std::optional<UnionView> union_view;
};

}  // namespace lyra::mir::interp
