#pragma once

#include <cstdint>
#include <limits>

namespace lyra::runtime {

class Scope;

// How far below a named instance a dump or an assertion control task reaches
// is counted in instances (LRM 21.7.1.2, 20.11): the instance named is the
// first level, and none is left at 0. This count reaches every level there is.
inline constexpr std::int64_t kEveryLevel =
    std::numeric_limits<std::int64_t>::max();

// The levels a task's `levels` argument asks for, where 0 is every level
// below and a count below 0 asks for none.
[[nodiscard]] auto LevelsNamed(std::int64_t levels) -> std::int64_t;

// Whether `scope` is a level of its own. A level is an instance: a block, task
// or function a scope holds is part of the instance holding it.
[[nodiscard]] auto CountsAsLevel(const Scope& scope) -> bool;

// The levels left once a walk with `remaining` enters `child`.
[[nodiscard]] auto LevelsBelow(const Scope& child, std::int64_t remaining)
    -> std::int64_t;

}  // namespace lyra::runtime
