#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::ast_to_hir {

// The one body two scopes of one text are, or nothing where they are not one.
// They are one where they differ in nothing but what their own constructions
// decide, at any depth beneath them, by an expression reading a value the
// construction is handed (LRM 27.4, 27.5): which alternative of a conditional
// stood, where the body holds every alternative either selected, and how many
// blocks a loop counted out, where the body holds the loop's body if either
// counted one.
[[nodiscard]] auto OneBodyOf(
    const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope>;

// The bodies a loop's blocks are, and which body the block at each index is,
// in the order the loop counts its blocks out.
struct LoopBodies {
  std::vector<hir::StructuralScope> bodies;
  std::vector<std::uint32_t> taken;
};

// The answer is read off what the blocks lowered to: every block is lowered
// with its index reaching it as a value its construction supplies, so two
// blocks that differ in nothing but what their constructions decide are one
// body. Blocks that are not are distinct bodies, each built at the indices
// that take it: blocks that are objects of different classes, and blocks of
// one class that still lowered apart, which are that class realized more than
// one way.
//
// The blocks carry no hierarchy index yet. A loop's blocks differ in it by
// definition, so it takes no part in the question.
[[nodiscard]] auto BodiesOf(std::span<const hir::StructuralScope> blocks)
    -> LoopBodies;

}  // namespace lyra::lowering::ast_to_hir
