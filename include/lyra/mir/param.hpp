#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct ParamId {
  std::uint32_t value;

  auto operator<=>(const ParamId&) const -> std::strong_ordering = default;
};

struct ParamDecl {
  std::string name;
  TypeId type;
};

struct ParamRef {
  EnclosingHops hops = {};
  ParamId param = {};
};

}  // namespace lyra::mir
