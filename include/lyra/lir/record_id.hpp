#pragma once

#include <compare>
#include <cstdint>

namespace lyra::lir {

struct RecordId {
  std::uint32_t value;

  auto operator<=>(const RecordId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
