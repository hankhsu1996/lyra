#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct MemberId {
  std::uint32_t value;

  auto operator<=>(const MemberId&) const -> std::strong_ordering = default;
};

struct Member {
  std::string name;
  TypeId type;
};

}  // namespace lyra::mir
