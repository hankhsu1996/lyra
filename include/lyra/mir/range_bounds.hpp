#pragma once

#include <variant>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

struct RangeConstantBounds {
  ExprId msb_expr;
  ExprId lsb_expr;
};

struct RangeIndexedUpBounds {
  ExprId base_index;
  ExprId width;
};

struct RangeIndexedDownBounds {
  ExprId base_index;
  ExprId width;
};

using RangeBounds = std::variant<
    RangeConstantBounds, RangeIndexedUpBounds, RangeIndexedDownBounds>;

}  // namespace lyra::mir
