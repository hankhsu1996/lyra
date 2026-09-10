#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Which of one class's introductions a behavior is, counted out of what that
// class published (LRM 8.20). Meaningless alone -- introduction 0 of one class
// is unrelated to introduction 0 of another -- which is why every use of one
// names the class beside it. A behavior a class of this unit introduces is
// named by that class's own callable instead, an identity no other unit can
// read.
struct BehaviorOrdinal {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BehaviorOrdinal&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
