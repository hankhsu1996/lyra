#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct LocalVarId {
  std::uint32_t value;

  auto operator<=>(const LocalVarId&) const -> std::strong_ordering = default;
};

struct LocalVar {
  std::string name;
  TypeId type;
};

}  // namespace lyra::mir
