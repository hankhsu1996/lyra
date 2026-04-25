#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct MemberVarId {
  std::uint32_t value;

  auto operator<=>(const MemberVarId&) const -> std::strong_ordering = default;
};

struct MemberVar {
  std::string name;
  TypeId type;
};

}  // namespace lyra::hir
