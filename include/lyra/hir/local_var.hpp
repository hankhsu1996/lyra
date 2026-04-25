#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct LocalVarId {
  std::uint32_t value;

  auto operator<=>(const LocalVarId&) const -> std::strong_ordering = default;
};

struct LocalVar {
  std::string name;
  TypeId type;
};

}  // namespace lyra::hir
