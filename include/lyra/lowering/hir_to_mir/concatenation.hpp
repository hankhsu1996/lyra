#pragma once

#include <span>

#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The type the bits of `parts` join into (LRM 11.4.12): as wide as every part
// together, unsigned, and able to hold x or z where any part can.
[[nodiscard]] auto JoinedBitsType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    std::span<const mir::ExprId> parts) -> mir::TypeId;

// Appends to `block` the declaration of `joined`, a local of the type `parts`
// join into, and one write of each part into the bits it takes, the first in
// the most significant ones, and answers with the read of `joined`. Each
// part's width is its type's, so where it lands is a constant of the program,
// and the steps stand beside each other however many parts there are, as a
// hand-written join inserts each part at its offset:
//
//   joined = 0; joined[55:48] = a; joined[47:32] = b; joined[31:0] = c
//
// What the value is then held to -- a declared type, or the unsigned vector
// LRM 11.8.1 makes of a source-level join -- is the caller's, reached by
// converting the read.
[[nodiscard]] auto BuildBitsJoin(
    mir::CompilationUnit& unit, mir::Block& block, mir::LocalId joined,
    std::span<const mir::ExprId> parts) -> mir::ExprId;

// The same, into a local the steps of `frame`'s block declare.
[[nodiscard]] auto BuildBitsJoin(
    mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const mir::ExprId> parts) -> mir::ExprId;

// Builds the string `parts` join into, their characters in the order given
// (LRM 11.4.12.2), by one call over one list holding every part, however many
// the source listed. One part is not a join, and the string is that part.
[[nodiscard]] auto BuildStringJoin(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> parts) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
