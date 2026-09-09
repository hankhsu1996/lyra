#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::lir {

struct ClosureId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ClosureId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
