#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Whether a value of `type` is too large to be kept in a frame, which is a
// stack no design chose the size of: such a value is held through a box the
// program allocates, and no body keeps one of its own.
auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool;

}  // namespace lyra::mir
