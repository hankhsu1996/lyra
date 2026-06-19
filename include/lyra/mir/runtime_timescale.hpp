#pragma once

#include <optional>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

// The four `$timeformat` argument expressions (LRM 20.4.3): display unit power,
// fractional-digit count, suffix string, and minimum field width.
struct TimeFormatArgExprs {
  ExprId units;
  ExprId precision;
  ExprId suffix;
  ExprId min_width;
};

// LRM 20.4.3: `$timeformat`. The four-argument form carries the argument
// expressions; the no-argument form is `nullopt` and restores the defaults
// (LRM Table 20-3). Arguments may be non-constant, so they stay as expressions
// and the runtime narrows them, matching how file descriptors flow.
struct RuntimeSetTimeFormatCall {
  std::optional<TimeFormatArgExprs> args;
};

}  // namespace lyra::mir
