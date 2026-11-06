#pragma once

#include <cstdint>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/region.hpp"

namespace lyra::runtime {

// The region a placement the compiler names outright crosses as, read back.
[[nodiscard]] inline auto RegionOperand(std::int64_t region)
    -> support::Region {
  if (region < 0 ||
      region > static_cast<std::int64_t>(support::Region::kPostponed)) {
    throw InternalError("RegionOperand: no region has this position");
  }
  return static_cast<support::Region>(region);
}

// A region later in the time step that a thread's own work lands in, named by
// what it is for in whichever region set the thread runs in (LRM 4.4.1): where
// a `#0` resumes it, and where its nonblocking assignments update.
enum class RegionInSet : std::uint8_t { kInactive, kNba };

// The regions of one region set. Design code runs in the active region set and
// a program's in the reactive one (LRM 24.3.1), each the other's dual.
struct RegionSet {
  support::Region active;
  support::Region inactive;
  support::Region nba;

  [[nodiscard]] constexpr auto operator[](RegionInSet which) const
      -> support::Region {
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
    .active = support::Region::kActive,
    .inactive = support::Region::kInactive,
    .nba = support::Region::kNba};
inline constexpr RegionSet kReactiveRegionSet{
    .active = support::Region::kReactive,
    .inactive = support::Region::kReInactive,
    .nba = support::Region::kReNba};

}  // namespace lyra::runtime
