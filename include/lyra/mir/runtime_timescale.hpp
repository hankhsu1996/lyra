#pragma once

#include <optional>
#include <string>

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

// LRM 20.4.2: `$printtimescale` (no-argument form). The scope name is the
// lexically enclosing design element, resolved at lowering; the unit and
// precision come from the scope class constants at render time.
struct RuntimePrintTimescaleCall {
  std::string scope_name;
};

}  // namespace lyra::mir
