#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::support {

// The strength scale LRM Table 28-7 numbers, which every contribution to a
// net's resolution sits at. Shared vocabulary: the compiler reads a level off
// what a declaration or an assignment states, and the runtime compares levels
// to decide which contribution determines a position. The four a driver can
// state (supply, strong, pull, weak) and the three a stored charge is held at
// (large, medium, small) interleave on one scale, which is why they are one
// enumeration rather than two.
//
// This is an ordered scale and so not a dispatch set: what reads it compares
// position -- where two contributions differ, the stronger determines the
// positions it drives (LRM 28.12.1) -- rather than naming a member. High
// impedance is the bottom, and a contribution there determines nothing.
enum class StrengthLevel : std::uint8_t {
  kHighImpedance,
  kSmall,
  kMedium,
  kWeak,
  kLarge,
  kPull,
  kStrong,
  kSupply,
};

inline constexpr std::size_t kStrengthLevelCount = 8;

}  // namespace lyra::support
