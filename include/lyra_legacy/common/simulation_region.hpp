#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::common {

/// Simulation regions per IEEE 1800-2017 Section 4.4
/// Excludes PLI-only regions (Pre-Active, Pre-NBA, Post-NBA, etc.)
enum class Region : uint8_t {
  kPreponed,    // #1step sampling
  kActive,      // Blocking assignments, always blocks
  kInactive,    // #0 delays
  kNBA,         // Nonblocking assignment updates
  kObserved,    // Assertion evaluation
  kReactive,    // Program blocks, assertion actions
  kReInactive,  // #0 in reactive context
  kReNBA,       // <= in reactive context
  kPostponed,   // $strobe, $monitor
};

/// Check if region is in the Active group (Active, Inactive, NBA)
constexpr auto IsActiveGroup(Region r) -> bool {
  return r == Region::kActive || r == Region::kInactive || r == Region::kNBA;
}

/// Check if region is in the Reactive group (Reactive, ReInactive, ReNBA)
constexpr auto IsReactiveGroup(Region r) -> bool {
  return r == Region::kReactive || r == Region::kReInactive ||
         r == Region::kReNBA;
}

/// Get region name for debug output
constexpr auto RegionName(Region r) -> std::string_view {
  switch (r) {
    case Region::kPreponed:
      return "Preponed";
    case Region::kActive:
      return "Active";
    case Region::kInactive:
      return "Inactive";
    case Region::kNBA:
      return "NBA";
    case Region::kObserved:
      return "Observed";
    case Region::kReactive:
      return "Reactive";
    case Region::kReInactive:
      return "ReInactive";
    case Region::kReNBA:
      return "ReNBA";
    case Region::kPostponed:
      return "Postponed";
  }
  return "Unknown";
}

}  // namespace lyra::common
