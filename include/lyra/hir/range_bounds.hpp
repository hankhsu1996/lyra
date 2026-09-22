#pragma once

#include <variant>

#include "lyra/hir/expr_id.hpp"

namespace lyra::hir {

// The two endpoints of a constant range-select `[left:right]`, in source syntax
// order (`sel.left()` / `sel.right()`). Which one is the physical low end is a
// property of the base container's orientation, decided at lowering -- not of
// the numeric magnitudes: for an ascending vector `[0:N]` the left endpoint is
// the least significant, for a descending `[N:0]` it is the most significant.
struct RangeConstantBounds {
  ExprId left_bound;
  ExprId right_bound;

  auto operator==(const RangeConstantBounds&) const -> bool = default;
};

struct RangeIndexedUpBounds {
  ExprId base_index;
  ExprId width;

  auto operator==(const RangeIndexedUpBounds&) const -> bool = default;
};

struct RangeIndexedDownBounds {
  ExprId base_index;
  ExprId width;

  auto operator==(const RangeIndexedDownBounds&) const -> bool = default;
};

using RangeBounds = std::variant<
    RangeConstantBounds, RangeIndexedUpBounds, RangeIndexedDownBounds>;

}  // namespace lyra::hir
