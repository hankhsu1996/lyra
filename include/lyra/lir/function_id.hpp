#pragma once

#include <compare>
#include <cstdint>

namespace lyra::lir {

// Identity of a function of this unit. Every body the unit compiles has one --
// a class's constructor, a method, a closure's invoke, a function no class
// lists -- because at this layer they are one thing: code with a signature,
// reached by name. What distinguishes a method is that its class lists it and
// its receiver arrives as its first parameter, neither of which is a property
// of the identity.
struct FunctionId {
  std::uint32_t value;

  auto operator<=>(const FunctionId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
