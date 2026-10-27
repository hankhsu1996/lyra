#pragma once

#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"

namespace lyra::mir_transform {

// One rewrite of MIR into MIR, named as its function is, as rustc names a pass
// by its type.
struct Pass {
  std::string_view name;
  auto (*run)(mir::CompilationUnit unit) -> mir::CompilationUnit;
};

// The passes that bring a unit from the phase it is in into `into`, which a
// correct program needs, as Swift's mandatory passes are the ones that commit a
// SIL stage: what a later stage reads is the form they leave, so no build skips
// them. They run in order, and the unit is in `into` once the last has.
struct Conversion {
  mir::Phase into;
  std::span<const Pass> passes;
};

// An optional rewrite a build asks to have run or skipped, by its name.
struct PassOverride {
  std::string name;
  bool runs;
};

// What a build asks of the rewrites beyond running them as the pipeline states:
// optional ones run or skipped, the last word on each winning, and the unit
// checked as lowered, after every optional rewrite and on reaching each phase
// rather than once, as it leaves the last.
struct PassOptions {
  std::vector<PassOverride> overrides;
  bool verify_each = false;
};

}  // namespace lyra::mir_transform
