#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

// Identity of a DPI-C import (LRM 35.4) within its structural scope's
// `foreign_imports` arena. A bodyless external callable, distinct from a
// body-bearing structural subroutine.
struct ForeignImportId {
  std::uint32_t value;

  auto operator<=>(const ForeignImportId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir
