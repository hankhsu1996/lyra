#pragma once

#include <vector>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

// Collect the sensitivity set for a process.
// ExternalRefId operands produce WaitTrigger entries with
// unresolved_external_ref set. These are resolved at backend
// normalization (FillTriggerArray) using ResolvedExternalRefBinding.
auto CollectSensitivity(const Process& process, const Arena& arena)
    -> std::vector<WaitTrigger>;

}  // namespace lyra::mir
