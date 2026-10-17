#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a structure or a union this compilation unit's source declares
// (LRM 7.2, 7.3). Unit-wide like `ClassId`, in its own registry. A record is a
// value, not a nominal object: it has no base and no dispatch, and its members
// are reached by position. A closure is a separate category (`ClosureId`).
struct RecordId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const RecordId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
