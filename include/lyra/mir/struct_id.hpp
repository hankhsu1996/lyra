#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a compiler-generated struct declaration within a compilation unit
// -- a nominal field-bearing aggregate synthesized by lowering for a promoted
// automatic scope. Unit-wide like `ClassId`, in its own registry. A generated
// struct is a type in the type system, not a nominal object: it has no base, no
// dispatch, and no lifecycle. A closure is a separate category (`ClosureId`).
struct StructId {
  std::uint32_t value;

  auto operator<=>(const StructId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
