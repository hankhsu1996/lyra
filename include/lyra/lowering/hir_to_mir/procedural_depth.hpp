#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/procedural_hops.hpp"

namespace lyra::lowering::hir_to_mir {

// Absolute procedural nesting depth during HIR-to-MIR lowering: 0 at a process
// or subroutine root, one deeper per nested block. It lives only in the
// lowering -- MIR references carry the relative ProceduralHops, never an
// absolute depth. The difference of two depths is exactly that hop count, so
// subtraction is the one operation that crosses from the lowering's depth into
// a MIR reference's hops; the strong type makes a depth/hops mix-up not
// compile.
struct ProceduralDepth {
  std::uint32_t value = 0;

  auto operator<=>(const ProceduralDepth&) const
      -> std::strong_ordering = default;

  // One scope deeper / shallower -- lowering enters and leaves scopes one level
  // at a time, and a capture's source is evaluated one scope outside the body.
  [[nodiscard]] auto Inner() const -> ProceduralDepth {
    return ProceduralDepth{.value = value + 1};
  }
  [[nodiscard]] auto Outer() const -> ProceduralDepth {
    return ProceduralDepth{.value = value - 1};
  }

  [[nodiscard]] auto operator-(ProceduralDepth rhs) const
      -> mir::ProceduralHops {
    return mir::ProceduralHops{.value = value - rhs.value};
  }
};

}  // namespace lyra::lowering::hir_to_mir
