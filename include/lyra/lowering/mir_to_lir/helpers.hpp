#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/mir/expression.hpp"

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

// Get the bit width of the element type after applying a given number of
// indices. For example, for `bit [1:0][1:0][7:0]` (32-bit total):
//   - depth=1 returns 16 (the bit width of `bit [1:0][7:0]`)
//   - depth=2 returns 8 (the bit width of `bit [7:0]`)
auto GetElementWidthAfterIndices(
    const common::Type& base_type, size_t num_indices) -> size_t;

// Compute a composite (flattened) index for multi-dimensional packed array
// access. For `bit [A][B][C]` with indices `[i][j]`:
//   composite = i * B + j
// The result can then be used to compute bit position: composite *
// element_width
auto ComputeCompositeIndex(
    const std::vector<std::unique_ptr<mir::Expression>>& indices,
    const common::Type& base_type, LirBuilder& builder) -> lir::TempRef;

}  // namespace lyra::lowering::mir_to_lir
