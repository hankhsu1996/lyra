#pragma once

#include <optional>
#include <span>

#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::ast_to_hir {

// The one body that serves every block of a loop, or nothing where the blocks
// are children in their own right.
//
// The answer is read off what the blocks lowered to: every block is lowered
// with its index reaching it as a value its construction supplies, so two
// blocks that differ in nothing else lower to the same scope. What they may
// still differ in is which alternative of a conditional written beneath them
// stood, at any depth, because what selects an alternative is an expression
// reading the index (LRM 27.4, 27.5). The one body then holds every
// alternative any block selected. Anything else that differs is a difference
// no construction can supply.
//
// The blocks carry no hierarchy index yet. A loop's blocks differ in it by
// definition, so it takes no part in the question.
[[nodiscard]] auto OneBodyOf(std::span<const hir::StructuralScope> blocks)
    -> std::optional<hir::StructuralScope>;

}  // namespace lyra::lowering::ast_to_hir
