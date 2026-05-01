#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct StructuralParamId {
  std::uint32_t value;

  auto operator<=>(const StructuralParamId&) const
      -> std::strong_ordering = default;
};

struct StructuralParamDecl {
  std::string name;
  TypeId type;
};

struct StructuralParamRef {
  StructuralHops hops = {};
  StructuralParamId param = {};
};

}  // namespace lyra::mir
