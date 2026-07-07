#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a closure declaration within a compilation unit -- an anonymous
// concrete callable value synthesized by lowering (capture fields plus one
// invoke body). Unit-wide like `ClassId` / `StructId`, in its own registry.
// Distinct per closure site: two closures of the same signature but different
// captures are different `ClosureId`s. A closure is not a nominal object and
// not a plain struct; it is its own callable-value category.
struct ClosureId {
  std::uint32_t value;

  auto operator<=>(const ClosureId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
