#pragma once

#include <string_view>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/phase.hpp"

namespace lyra::mir {

struct Block;
struct CompilationUnit;

// Checks what this layer states about the bodies of a unit in the phase it is
// in, and throws InternalError naming the body that breaks it and `when` the
// unit was checked.
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
void Verify(const CompilationUnit& unit, std::string_view when);

// The same for `code`, a body at `at` that `unit` does not hold, which stands
// on what the unit states and is checked as a body the unit holds is in
// `phase`.
void VerifyBody(
    const CompilationUnit& unit, Phase phase, const CodeSource& at,
    const CallableCode& code, std::string_view when);

// Refuses a unit `reader` is handed that is not legalized, which is the one
// phase a backend reads: anything earlier still holds what only a conversion
// removes.
void RequireLegalized(const CompilationUnit& unit, std::string_view reader);

// Whether evaluating the node `id` names in `block` computes nothing: it names
// a declared thing, spells a constant, or forms a place over nodes of which the
// same holds. Such a node may be reached from any number of places, so a
// lowering that needs a value at several asks this before binding it to a
// local.
[[nodiscard]] auto EvaluatesNothing(const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
