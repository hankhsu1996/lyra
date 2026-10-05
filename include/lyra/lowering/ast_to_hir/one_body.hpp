#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::ast_to_hir {

// The bodies a loop's blocks are, and which body the block at each index is,
// in the order the loop counts its blocks out.
struct LoopBodies {
  std::vector<hir::StructuralScope> bodies;
  std::vector<std::uint32_t> taken;
};

// The answer is read off what the blocks lowered to: every block is lowered
// with its index reaching it as a value its construction supplies, so two
// blocks that differ in nothing else lower to the same scope. What they may
// still differ in is which alternative of a conditional written beneath them
// stood, at any depth, because what selects an alternative is an expression
// reading the index (LRM 27.4, 27.5); a body then holds every alternative any
// of its blocks selected. Blocks that differ in anything else -- a width their
// index fixes -- are distinct bodies, each built at the indices that take it.
//
// The blocks carry no hierarchy index yet. A loop's blocks differ in it by
// definition, so it takes no part in the question.
[[nodiscard]] auto BodiesOf(std::span<const hir::StructuralScope> blocks)
    -> LoopBodies;

}  // namespace lyra::lowering::ast_to_hir
