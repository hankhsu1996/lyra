#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

namespace lyra::mir {

// Identity of a class declaration within a compilation unit -- a module, a
// named generate scope, or a SystemVerilog class. A reference to a class (a
// member's pointee, a constructed type, a receiver type) names this id, and the
// unit's class registry resolves it to the declaration. Unlike the scope-local
// arena ids (an expression, a statement, a member), this id is unit-wide: it
// resolves without holding the class's container, since a class can be named
// from a position that does not enclose it.
struct ClassId {
  std::uint32_t value;

  auto operator<=>(const ClassId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir

// A `ClassId` is a value identity, so it keys hashed containers directly rather
// than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::mir::ClassId> {
  auto operator()(lyra::mir::ClassId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
