#pragma once

#include <cstdint>
#include <optional>

#include "lyra/support/data_object.hpp"

namespace lyra::hir {

// A data object's type as the source declared it (LRM 37.25), which a reader
// reaching the design by name reports, apart from the type its values have: an
// `int` and a `bit signed [31:0]` hold the same values and are two typespecs.
// It states what kind of type it is, how many bits a value of it holds, the
// outermost packed range a vector declares, and, for an enumeration, its base
// type (LRM 6.19).
struct Typespec {
  support::TypespecKind kind = support::TypespecKind::kLogic;
  std::uint32_t width = 0;
  std::optional<support::Range> range;
  // An enumeration's base is an integral type as wide as the enumeration,
  // which has no base of its own; `int` where the source states none.
  struct Base {
    support::TypespecKind kind;
    std::optional<support::Range> range;

    auto operator==(const Base&) const -> bool = default;
  };
  std::optional<Base> base;

  auto operator==(const Typespec&) const -> bool = default;
};

}  // namespace lyra::hir
