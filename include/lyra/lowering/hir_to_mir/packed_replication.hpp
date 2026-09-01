#pragma once

#include <cstdint>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the value a packed run repeated `count` times denotes (LRM 11.4.12.1),
// landing in the type given.
//
// The count is a number rather than an operand because LRM 11.4.12.1 requires
// a constant multiplier over packed operands, and knowing it is what settles
// the degenerate case: repeating once is not a repetition, and the value is
// that run seen at the result type. Saying so here is what lets the repeat
// entry mean a repeat, so no consumer of it has to read a count of one as a
// value that merely wraps another. A multiplier the run evaluates -- which
// LRM 11.4.12.2 allows over a string -- cannot be told apart this way and
// reaches the entry directly.
//
// `block` is the destination scope for any intermediate expression this
// interns.
[[nodiscard]] auto BuildPackedReplication(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    std::uint64_t count, mir::TypeId result_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
