#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct StructuralVarId {
  std::uint32_t value;

  auto operator<=>(const StructuralVarId&) const
      -> std::strong_ordering = default;
};

struct StructuralVarDecl {
  std::string name;
  TypeId type;
};

}  // namespace lyra::mir
