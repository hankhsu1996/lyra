#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a callable in its unit: its entry in the unit's one table of
// callable declarations, whoever declares it -- a class, its construction, the
// unit's namespace, a closure or a record. Meaningless beside another unit's.
struct CallableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CallableId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir

// A `CallableId` is a value identity, so it keys hashed containers directly
// rather than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::mir::CallableId> {
  auto operator()(lyra::mir::CallableId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
