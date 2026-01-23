#pragma once

#include <vector>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

// Collect the sensitivity set for a process: all design slots read
// by any instruction in any block. Used for always_comb/always_latch
// to synthesize the implicit sensitivity list.
// Returns triggers with kAnyChange edge for each unique design slot.
auto CollectSensitivity(const Process& process, const Arena& arena)
    -> std::vector<WaitTrigger>;

}  // namespace lyra::mir
