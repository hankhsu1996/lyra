#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/phase.hpp"

namespace lyra::mir {

struct Block;
struct CompilationUnit;

// Which checks a verification makes. Every build checks what one node or one
// statement states, at a cost that follows the unit's size. What holds only
// across a whole body or the unit -- the order a target runs operands in, a
// computation reached once, a move as a local's last use, the bytes a frame
// keeps together, what a block expression yields outliving it, a closure kept
// past its frame -- is worked out again over all of it, so it is checked where
// a run asks for every check, as rustc's validator makes the checks it calls
// expensive only under `-Zvalidate-mir`, its dataflow checks are a pass of
// their own under `-Zlint-mir`, and LLVM builds its expensive checks only
// where asked.
enum class Checks : std::uint8_t { kEveryBuild, kAll };

// Checks what this layer states about the bodies of a unit in the phase it is
// in, making `checks`, and throws InternalError naming the body that breaks it
// and `when` the unit was checked.
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
// refused here -- the second where `checks` asks for every check -- while the
// body is still named after what the source wrote, rather than as a
// machine-level fault one lowering later, as another compiler's error against
// generated text, or as an operand that runs twice.
void Verify(const CompilationUnit& unit, std::string_view when, Checks checks);

// The same for `code`, a body at `at` that `unit` does not hold, which stands
// on what the unit states and is checked as a body the unit holds is in
// `phase`.
void VerifyBody(
    const CompilationUnit& unit, Phase phase, const CodeSource& at,
    const CallableCode& code, std::string_view when, Checks checks);

// Refuses a unit `reader` is handed that is not legalized, which is the one
// phase a backend reads: anything earlier still holds what only a conversion
// removes.
void RequireLegalized(const CompilationUnit& unit, std::string_view reader);

// Whether evaluating the node `id` names in `block` computes nothing: it names
// a declared thing, spells a constant, reads a value where it lies in storage
// only a binding of the body or a constant reaches, or forms a place over
// nodes of which the same holds -- a place reached through a class handle
// excepted, since opening the handle can fail the design. Such a node may be
// reached from any number of places, so a lowering that needs a value at
// several asks this before binding it to a local.
[[nodiscard]] auto EvaluatesNothing(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
