#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct MethodId {
  std::uint32_t value;

  auto operator<=>(const MethodId&) const -> std::strong_ordering = default;
};

// A method of a class, named by its class-local id. The class a call reaches is
// determined by the call's receiver (the method belongs to the receiver's
// class), not carried here -- a reference names the method, not its owner.
struct MethodRef {
  MethodId method = {};
};

}  // namespace lyra::mir
