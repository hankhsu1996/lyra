#pragma once

// What the source leaves unwritten about where it is written: the clock in
// force (LRM 16.9.3, 16.16), the default clocking of a scope it falls back to
// (LRM 14.12), the default disable condition (LRM 16.15), and the two
// functions that name the clock or the condition as a formal's default (LRM
// 16.14.7).

#include <concepts>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ClockingBlockSymbol;
class Expression;
class Scope;
class SignalEventControl;
class Symbol;
class TimingControl;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The innermost sequence or property instance being expanded whose declaration
// was reached by a hierarchical name or through an interface port and stands
// where `declared` stands, out of any clocking block: a name the body writes
// binds where the declaration is (LRM 16.8, 11.12), so `declared` is a
// declaration of the instance that name led to. Nothing where no such instance
// encloses what is being lowered.
auto ExpansionDeclaringBeside(
    const WalkFrame& frame, const slang::ast::Symbol& declared)
    -> const ExpansionInstance*;

// The events of a clocking block's clocking event, in the order its event list
// wrote them (LRM 14.3, 9.4.2.1); one written alone is a list of one.
auto ClockingEventsOf(const slang::ast::ClockingBlockSymbol& block)
    -> std::vector<const slang::ast::SignalEventControl*>;

// The event of the default clocking in force at `scope`, or nothing where no
// default clocking is declared there.
auto DefaultClockingEvent(const slang::ast::Scope& scope)
    -> const slang::ast::TimingControl*;

// The clock in force where an expression is lowered, by LRM 16.9.3's ordered
// rules: the clock of the assertion whose property or action block holds it,
// then the clock an enclosing procedure settles (LRM 16.14.6), and otherwise
// the default clocking of the scope it is written in (LRM 14.12). Nothing where
// no rule yields one.
template <typename Lowerer>
auto ClockInForce(Lowerer& lowerer, const WalkFrame& frame)
    -> std::optional<ClockingEvent> {
  if (frame.assertion_clock.has_value()) {
    return frame.assertion_clock;
  }
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    if (const auto* clock =
            lowerer.Owner().InferredProcedureClock(lowerer.ContainingSymbol());
        clock != nullptr) {
      return ClockingEvent{clock};
    }
  }
  if (const auto* clock = DefaultClockingEvent(*frame.reader_scope)) {
    return ClockingEvent{clock};
  }
  return std::nullopt;
}

// One entry of an event list a clock stands for (LRM 9.4.2.1): an event the
// source wrote, or the clocking event of another instance's clocking block,
// whose own events that instance publishes.
using ClockEntry =
    std::variant<const slang::ast::SignalEventControl*, ClockOfAnotherInstance>;

// The entries a clocking event stands for: a list flattened into its entries,
// and each `$inferred_clock` replaced by the entries of the clock in force
// where the sequence or property whose formal it defaults is instantiated (LRM
// 16.14.7), found among the instances `frame` records. One whose instance
// stands where no clock is in force is refused, as a construct no rule gives a
// clock is.
auto ResolvedClockEntries(
    const WalkFrame& frame, const ClockingEvent& clock, diag::SourceSpan span)
    -> diag::Result<std::vector<ClockEntry>>;

// Whether two clocking events are one clock where `frame` stands: the same
// entries, two the source wrote compared as the front end compares two events
// (LRM 16.13.3), and two of another instance's clocking blocks the same where
// they are one block of one instance.
auto IsSameClock(
    const WalkFrame& frame, const ClockingEvent& first,
    const ClockingEvent& second, diag::SourceSpan span) -> diag::Result<bool>;

// The condition of the `default disable iff` in force at `scope`: the
// innermost one among it and the scopes it is declared in, which reaches into
// a generate block and a nested declaration and never into an instance.
// Nothing where none is in force.
auto DefaultDisableCondition(const slang::ast::Scope& scope)
    -> const slang::ast::Expression*;

// The condition a disable condition the source wrote stands for: itself, or,
// for `$inferred_disable`, the default disable condition in force at
// `assertion_scope`, the scope the assertion stands in however deep the
// instance whose formal it defaults, and nothing where none is in force there
// (LRM 16.14.7).
auto ResolveWrittenDisable(
    const slang::ast::Scope& assertion_scope,
    const slang::ast::Expression& written) -> const slang::ast::Expression*;

}  // namespace lyra::lowering::ast_to_hir
