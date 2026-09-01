#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct PatternId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PatternId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
