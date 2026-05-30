#pragma once

#include <variant>

#include "lyra/hir/expr_id.hpp"

namespace lyra::hir {

struct InsideRangePair {
  ExprId lo;
  ExprId hi;
};

using InsideItem = std::variant<ExprId, InsideRangePair>;

}  // namespace lyra::hir
