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

// Generalized blob view for accessing union storage as typed regions (const).
// Replaces the old ConstUnionView. Once in blob mode, all nested projections
// (field, index) adjust bit_offset rather than navigating pointers.
struct ConstBitBlobView {
  const RuntimeIntegral* storage;
  uint32_t bit_offset;
  TypeId view_type;        // Current type being viewed within the blob
  TypeId root_union_type;  // Outermost union; determines contains_float policy
};

// Generalized blob view for accessing union storage as typed regions (mutable).
struct BitBlobView {
  RuntimeIntegral* storage;
  uint32_t bit_offset;
  TypeId view_type;        // Current type being viewed within the blob
  TypeId root_union_type;  // Outermost union; determines contains_float policy
};

// Location for read operations (const pointer to base).
struct ConstLocation {
  const RuntimeValue* base = nullptr;
  std::optional<BitSlice> bit_slice;
  std::optional<ConstBitBlobView> blob_view;
};

// Location for write operations (mutable pointer to base).
struct Location {
  RuntimeValue* base = nullptr;
  std::optional<BitSlice> bit_slice;
  std::optional<BitBlobView> blob_view;
};

}  // namespace lyra::mir::interp
