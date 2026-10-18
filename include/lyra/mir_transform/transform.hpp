#pragma once

#include <span>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// The rewrites between the lowering that states what the source means and
// every backend reading it, in the order they run.
[[nodiscard]] auto Pipeline() -> std::span<const Pass>;

// MIR rewritten into MIR, one unit at a time, by the pipeline. Each rewrite
// keeps what the program does and says it only in MIR's own vocabulary, so
// both backends translate the result as they translate anything else and
// neither knows a rewrite ran. Every optional rewrite runs unless `options`
// skips it, and the unit is checked as it leaves the last one, since that is
// the form every consumer reads -- and after each one as well where `options`
// asks.
auto Transform(mir::CompilationUnit unit, const PassOptions& options)
    -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
