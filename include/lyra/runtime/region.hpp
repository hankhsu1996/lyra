#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::runtime {

// The ordered regions of one time slot (LRM 4.4). The standard's PLI regions
// are absent because there is no PLI to schedule callbacks for, leaving the
// simulation regions of LRM 4.4.2 in the order LRM 4.4 lists them.
//
// The order is the content: a slot runs its regions by ascending value and LRM
// 4.5 asks for the first nonempty region of a range, so consumers compare by
// position, which makes this an ordered scale and not a dispatch set.
enum class Region : std::uint8_t {
  kPreponed,
  kActive,
  kInactive,
  kNba,
  kObserved,
  kReactive,
  kReInactive,
  kReNba,
  kPostponed,
};

inline constexpr std::size_t kRegionCount =
    static_cast<std::size_t>(Region::kPostponed) + 1;

}  // namespace lyra::runtime
