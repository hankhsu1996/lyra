#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/enclosing_hops.hpp"

namespace lyra::mir {

struct MethodId {
  std::uint32_t value;

  auto operator<=>(const MethodId&) const -> std::strong_ordering = default;
};

struct MethodRef {
  EnclosingHops hops = {};
  MethodId method = {};
};

}  // namespace lyra::mir
