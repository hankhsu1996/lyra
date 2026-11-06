#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

// A clocking block a structural scope declares (LRM 14.3), by its position
// among that scope's blocks.
struct ClockingBlockId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ClockingBlockId&) const
      -> std::strong_ordering = default;
};

// A clockvar of a clocking block (LRM 14.3), by its position among the
// block's.
struct ClockvarId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ClockvarId&) const -> std::strong_ordering = default;
};

// What lands the drives of one output of a clocking block (LRM 14.16), named by
// the block and the output: a callable the scope declaring the block publishes
// and defines, though the source wrote none, since the output's skew is that
// instance's (LRM 14.4). It stands beside the ids because both the drive's
// control and the publication name it.
struct ClockingOutputLanding {
  ClockingBlockId block;
  ClockvarId clockvar;

  auto operator==(const ClockingOutputLanding&) const -> bool = default;
};

}  // namespace lyra::hir
