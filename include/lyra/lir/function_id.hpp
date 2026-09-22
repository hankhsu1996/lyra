#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

#include "lyra/base/pool_id.hpp"

namespace lyra::lir {

// Identity of a function of this unit. Every body the unit compiles has one --
// a class's constructor, a method, a closure's invoke, a function no class
// lists -- because at this layer they are one thing: code with a signature,
// reached by name. What distinguishes a method is that its class lists it and
// its receiver arrives as its first parameter, neither of which is a property
// of the identity.
struct FunctionId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const FunctionId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir

// A `FunctionId` is a function identity, so it keys hashed containers directly
// rather than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::FunctionId> {
  auto operator()(lyra::lir::FunctionId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
