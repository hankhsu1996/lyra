#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

// Identifies a loop as a non-local break target. A `foreach` lowers to nested
// loops, so a `break` that exits the whole foreach must name the outermost
// loop -- the universal labeled-break primitive (Java/Go/Rust loop labels,
// LLVM/Wasm branch targets). Minted per procedural body via AddLoopLabel.
struct LoopLabelId {
  std::uint32_t value;

  auto operator<=>(const LoopLabelId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
