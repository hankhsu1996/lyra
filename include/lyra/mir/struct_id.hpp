#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a struct this compilation unit declares, whether the source
// declared it (LRM 7.2) or a lowering made it for the locals a scope keeps past
// its end (LRM 6.21). Unit-wide like `ClassId`, in its own registry. A struct
// is a value, not a nominal object: it has no base, no dispatch, and no
// lifecycle, and its members are reached by position. A closure is a separate
// category (`ClosureId`).
struct StructId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StructId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
