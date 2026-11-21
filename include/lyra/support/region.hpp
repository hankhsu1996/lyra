#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::support {

// The ordered regions of one time slot (LRM 4.4): the simulation regions of
// LRM 4.4.2, and the PLI regions of LRM 4.4.3 that LRM Table 4-1 places a
// callback in (LRM 4.10). The PLI regions no callback reason names --
// Pre-Observed, Post-Observed, Pre-Re-NBA, Post-Re-NBA -- would hold nothing,
// so they are not here. A placement the standard names outright crosses from
// the compiler to the engine as one of these.
//
// The order is the content: a slot runs its regions by ascending value and LRM
// 4.5 asks for the first nonempty region of a range, so consumers compare by
// position, which makes this an ordered scale and not a dispatch set.
enum class Region : std::uint8_t {
  kPreponed,
  kPreActive,
  kActive,
  kInactive,
  kPreNba,
  kNba,
  kPostNba,
  kObserved,
  kReactive,
  kReInactive,
  kReNba,
  kPrePostponed,
  kPostponed,
};

inline constexpr std::size_t kRegionCount =
    static_cast<std::size_t>(Region::kPostponed) + 1;

}  // namespace lyra::support
