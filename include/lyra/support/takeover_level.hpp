#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::support {

// The two levels LRM 10.6 gives a procedural continuous assignment, in
// increasing precedence. Shared vocabulary: the compiler encodes which keyword
// took a target over, and the runtime reads it to decide what a value arriving
// at a target displaces. An `assign` (LRM 10.6.1) outranks the procedural
// writes a target takes on its own; a `force` (LRM 10.6.2) outranks both, and
// on a net outranks every driver. The language defines no third level.
//
// Not a dispatch set: it is an ordered scale, and what reads it compares
// position -- which level outranks which -- rather than naming a member. Its
// values are contiguous from zero and the count below bounds them, so a target
// holds one slot per level and answers with the highest occupied one.
enum class TakeoverLevel : std::uint8_t {
  kAssign,
  kForce,
};

inline constexpr std::size_t kTakeoverLevelCount = 2;

}  // namespace lyra::support
