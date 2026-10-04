#pragma once

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

struct Block;
struct CompilationUnit;

// Checks what this layer states about the bodies of a unit, where the unit is
// produced, and throws InternalError naming the body that breaks it.
//
// A body's result type is its call protocol, so a body that is not a coroutine
// returns to its caller before anything else runs and cannot suspend: nothing
// would resume it.
//
// No run evaluates a node that computes twice. Every consumer evaluates a node
// at each place that reaches it, so a node reached at two places one run can
// both take is two evaluations of what the lowering stated once.
//
// Every layer below consumes both facts, and a lowering that breaks one is
// refused here, where the body is still named after what the source wrote,
// rather than as a machine-level fault one lowering later, as another
// compiler's error against generated text, or as an operand that runs twice.
void Verify(const CompilationUnit& unit);

// Whether evaluating the node `id` names in `block` computes nothing: it names
// a declared thing, spells a constant, or forms a place over nodes of which the
// same holds. Such a node may be reached from any number of places, so a
// lowering that needs a value at several asks this before binding it to a
// local.
[[nodiscard]] auto EvaluatesNothing(const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
