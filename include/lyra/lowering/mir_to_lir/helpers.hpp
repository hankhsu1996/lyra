#pragma once

#include <cstdint>

#include "lyra/lir/context.hpp"

namespace lyra::lowering::mir_to_lir {

class LirBuilder;

// Adjust an index/offset for non-zero-based integral types (e.g., bit [63:32]).
// Returns the adjusted index if lower_bound != 0, otherwise returns the
// original.
//
// This helper is used when accessing packed array elements or slices where the
// index range doesn't start at zero. For example, `bit [63:32] data` has
// indices 32-63, so accessing `data[40]` requires subtracting 32 from the
// index.
auto AdjustForNonZeroLower(
    lir::TempRef index, int32_t lower_bound, LirBuilder& builder)
    -> lir::TempRef;

}  // namespace lyra::lowering::mir_to_lir
