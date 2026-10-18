#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"

namespace lyra::mir_transform {

// Whether a correct program needs a rewrite, as rustc's `PassPolicy` and LLVM's
// required passes say. A required one brings a unit into a form its consumers
// need, so no build skips it; an optional one leaves the same program at
// another cost, so skipping it changes nothing the program does.
enum class PassPolicy : std::uint8_t { kRequired, kOptional };

// Whether a build may skip a rewrite under `policy`.
[[nodiscard]] inline auto MayBeSkipped(PassPolicy policy) -> bool {
  switch (policy) {
    case PassPolicy::kRequired:
      return false;
    case PassPolicy::kOptional:
      return true;
  }
  throw InternalError("a rewrite states no policy");
}

// One rewrite of MIR into MIR, named as its function is, as rustc names a pass
// by its type.
struct Pass {
  std::string_view name;
  PassPolicy policy;
  auto (*run)(mir::CompilationUnit unit) -> mir::CompilationUnit;
  // The phase a unit is in once this rewrite has run, where it narrows what
  // the unit may hold.
  std::optional<mir::Phase> phase_after;
};

// An optional rewrite a build asks to have run or skipped, by its name.
struct PassOverride {
  std::string name;
  bool runs;
};

// What a build asks of the rewrites beyond running them as the pipeline states:
// optional ones run or skipped, the last word on each winning, and the unit
// checked after every rewrite rather than once, as it leaves the last.
struct PassOptions {
  std::vector<PassOverride> overrides;
  bool verify_each = false;
};

}  // namespace lyra::mir_transform
