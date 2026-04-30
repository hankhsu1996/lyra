#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct ProceduralVarId {
  std::uint32_t value;

  auto operator<=>(const ProceduralVarId&) const
      -> std::strong_ordering = default;
};

struct ProceduralVarDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::mir
