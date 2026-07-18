#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a callable a namespace owns -- a class's instance method, a
// class's or a package's receiver-less static callable, a DPI-C import. One
// identity for the one callable concept, scoped to the class or unit that
// declares it.
struct CallableId {
  std::uint32_t value;

  auto operator<=>(const CallableId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
