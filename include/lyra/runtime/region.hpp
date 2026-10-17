#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>

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

// A region later in the time step that a thread's own work lands in, named by
// what it is for in whichever region set the thread runs in (LRM 4.4.1): where
// a `#0` resumes it, and where its nonblocking assignments update.
enum class RegionInSet : std::uint8_t { kInactive, kNba };

// The regions of one region set. Design code runs in the active region set and
// a program's in the reactive one (LRM 24.3.1), each the other's dual.
struct RegionSet {
  Region active;
  Region inactive;
  Region nba;

  [[nodiscard]] constexpr auto operator[](RegionInSet which) const -> Region {
    switch (which) {
      case RegionInSet::kInactive:
        return inactive;
      case RegionInSet::kNba:
        return nba;
    }
    std::unreachable();
  }
};

inline constexpr RegionSet kActiveRegionSet{
    .active = Region::kActive,
    .inactive = Region::kInactive,
    .nba = Region::kNba};
inline constexpr RegionSet kReactiveRegionSet{
    .active = Region::kReactive,
    .inactive = Region::kReInactive,
    .nba = Region::kReNba};

}  // namespace lyra::runtime
