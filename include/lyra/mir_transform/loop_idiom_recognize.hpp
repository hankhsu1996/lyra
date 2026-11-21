#pragma once

#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// Writes the run a loop moves one bit at a time as one assignment of a select
// as wide as the run, as LLVM's LoopIdiomRecognize replaces a loop storing
// what it loads with one operation over the whole run. The run is as wide as
// the loop makes passes, which the variable's start and the bound fix before
// the program runs, so the select is of a type like any other. A packed value
// is one vector (LRM 7.4.1) and a select of each bit of it is valid or not by
// itself (LRM 11.5.1), as each bit of a wider select is, so writing the run at
// once leaves every bit as the loop would wherever no position wraps over the
// run and nothing a pass reads is what an earlier pass wrote. Whether that
// holds is checked where the loop starts, while the program runs, and the
// loop as written follows the check: it makes no pass where the run was
// written and every pass where it was not, as LLVM's vectorizer keeps the
// original scalar loop for when its runtime checks fail.
auto LoopIdiomRecognizeOnBody(
    const PassContext& context, const mir::CallableCode& code)
    -> std::optional<mir::CallableCode>;

}  // namespace lyra::mir_transform
