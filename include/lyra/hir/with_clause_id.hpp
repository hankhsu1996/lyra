#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

// Identity of one LRM 7.12 array-method `with` clause, assigned per clause at
// AST-to-HIR. A reference to the clause's element or index names this identity
// (not a nesting position), so the binding a reference points at stays stable
// however deeply the referencing clause is nested.
struct WithClauseId {
  std::uint32_t value;

  auto operator<=>(const WithClauseId&) const -> std::strong_ordering = default;
};

// The two values an LRM 7.12 array-method `with` clause provides per iteration:
// the element (`item`) and its index / key (`item.index`).
enum class IterationBindingRole : std::uint8_t { kElement, kIndex };

}  // namespace lyra::hir
