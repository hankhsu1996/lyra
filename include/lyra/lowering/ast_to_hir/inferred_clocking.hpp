#pragma once

// What the source leaves unwritten about where it is written: the clock in
// force (LRM 16.9.3, 16.16), the default clocking of a scope it falls back to
// (LRM 14.12), the default disable condition (LRM 16.15), and the two
// functions that name the clock or the condition as a formal's default (LRM
// 16.14.7).

#include <concepts>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class Scope;
class SignalEventControl;
class TimingControl;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

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
    -> const slang::ast::TimingControl* {
  if (frame.assertion_clock != nullptr) {
    return frame.assertion_clock;
  }
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    if (const auto* clock =
            lowerer.Owner().InferredProcedureClock(lowerer.ContainingSymbol());
        clock != nullptr) {
      return clock;
    }
  }
  return DefaultClockingEvent(*frame.reader_scope);
}

// The events a clocking event the source wrote stands for, as the entries of
// one event list (LRM 9.4.2.1): a list flattened into its entries, and each
// `$inferred_clock` replaced by the entries of the clock in force where the
// sequence or property whose formal it defaults is instantiated (LRM 16.14.7),
// found among the instances `frame` records. One whose instance stands where
// no clock is in force is refused, as a construct no rule gives a clock is.
auto ResolvedClockEntries(
    const WalkFrame& frame, const slang::ast::TimingControl& written,
    diag::SourceSpan span)
    -> diag::Result<std::vector<const slang::ast::SignalEventControl*>>;

// Whether two clocking events the source wrote are one clock where `frame`
// stands: the same entries, each as the front end compares two events (LRM
// 16.13.3).
auto IsSameClock(
    const WalkFrame& frame, const slang::ast::TimingControl& first,
    const slang::ast::TimingControl& second, diag::SourceSpan span)
    -> diag::Result<bool>;

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
