#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/event_edge.hpp"

namespace lyra::hir {

struct DelayControl {
  ExprId duration;

  auto operator==(const DelayControl&) const -> bool = default;
};

// A leaf reads the whole of its cell.
struct WatchedWhole {
  auto operator==(const WatchedWhole&) const -> bool = default;
};

// A leaf reads the part of its cell the source selected: the longest static
// prefix the source wrote (LRM 11.5.3), a select over the cell whose indices
// are constants. A genvar or a parameter is such a constant and each
// construction is given its own, so the part is evaluated where the leaf is.
struct WatchedSelect {
  ExprId prefix;

  auto operator==(const WatchedSelect&) const -> bool = default;
};

using WatchedPart = std::variant<WatchedWhole, WatchedSelect>;

// Where a read set's storage is, fixed for as long as a wait on it lasts: a
// cell elaboration sealed; or a variable the running body declares, which lives
// as long as a call or a block and so has no place on the design hierarchy to
// be routed to (LRM 6.21) -- where it is a `ref` formal, what it watches is
// whatever its actual reports to (LRM 13.5.2).
using WatchedStorage = std::variant<ValueTarget, ProceduralVarRef>;

// One cell of a read set, and which part of it the read reaches.
struct SensitivityEntry {
  WatchedStorage cell;
  WatchedPart part;

  auto operator==(const SensitivityEntry&) const -> bool = default;
};

// The cells a body's text reads and writes, which are the same however it
// runs, and so are stated where it is entered.
//
// A waited evaluation is handed these from every function and construction it
// runs, beside what the body reaches through a handle, which it states as it
// reaches it (LRM 9.4.2). An `always_comb` / `always_latch` collects its
// implicit list from them once, before it first runs (LRM 9.2.2.2.1): it
// follows `calls` without running any body, and leaves out `writes` (exception
// b). `calls` are the calls of functions the clause analyzes as functions --
// never one made on a class object, which adds nothing to the list but its
// arguments, nor one through a virtual interface.
//
// `unreportable` says why the cells cannot be stated, where the body reads
// something no wait can watch yet. A body is compiled whether or not a wait
// reaches it, so that is not an error of the body's; it is raised where a wait
// asks.
struct Reads {
  std::vector<SensitivityEntry> cells;
  std::vector<SensitivityEntry> writes;
  std::vector<ExprId> calls;
  std::optional<std::string> unreportable;

  auto operator==(const Reads&) const -> bool = default;
};

// One entry of an explicit `@(...)` event control (LRM 9.4.2). `signal` is the
// expression the event is a change in the value of, `edge` the direction its
// least significant bit must take where one was written, and `cells` the
// storage elaboration sealed that it reads -- a change to any of which is a
// candidacy for the event rather than the event itself. What it reaches
// through a handle, an interface or a call is the expression's own nodes,
// found as an evaluation reaches them.
//
// `condition` is the LRM 9.4.2.3 `iff` qualifier: a value change is an event
// only while it holds. It is read when the watched expression moves, and what
// it reads is no part of what the entry is sensitive to, because the standard
// evaluates it then and not when the qualifier itself moves.
struct EventTrigger {
  ExprId signal;
  support::EventEdge edge{};
  std::vector<SensitivityEntry> cells;
  std::optional<ExprId> condition;

  auto operator==(const EventTrigger&) const -> bool = default;
};

struct EventControl {
  std::vector<EventTrigger> triggers;

  auto operator==(const EventControl&) const -> bool = default;
};

// LRM 9.4.2.2 `@*` / `@(*)`: the controlled statement waits on what its text
// reads.
struct ImplicitEventControl {
  std::vector<SensitivityEntry> sensitivity_list;

  auto operator==(const ImplicitEventControl&) const -> bool = default;
};

// LRM 15.5.2 `@e;`. What the wait watches is the event rather than the value of
// an expression, so the entry names storage and nothing else -- a trigger is
// the event itself, and there is no value to have moved. `condition` is the LRM
// 9.4.2.3 `iff` qualifier, read where the trigger happens.
struct NamedEventControl {
  SensitivityEntry event;
  std::optional<ExprId> condition;

  auto operator==(const NamedEventControl&) const -> bool = default;
};

using TimingControl = std::variant<
    DelayControl, EventControl, ImplicitEventControl, NamedEventControl>;

// An `event_control` in whichever of its two forms the source wrote (LRM A.6.5)
// -- a change in the value of an expression, or a named event's trigger. The
// name says "any" because `EventControl` alone is the value-change form.
using AnyEventControl = std::variant<EventControl, NamedEventControl>;

// LRM 9.4.5 `repeat (n) @(...)`: the effect waits for that many occurrences of
// the event. The grammar puts a repeat count only in front of an event control,
// and only where a whole `delay_or_event_control` may stand.
struct RepeatedEventControl {
  ExprId count;
  AnyEventControl event;

  auto operator==(const RepeatedEventControl&) const -> bool = default;
};

// LRM A.6.5 `delay_or_event_control`: what names the slot an effect is due in,
// where the source wrote one. It is the timing controls a statement may be
// prefixed with, minus the implicit `@*` list -- which the grammar admits only
// in front of a statement -- plus the repeat form, which only this position
// admits. A nonblocking assignment (LRM 10.4.2) and a nonblocking event trigger
// (LRM 15.5.1) each carry one.
using DelayOrEventControl = std::variant<
    DelayControl, EventControl, NamedEventControl, RepeatedEventControl>;

// The effect happens where the statement is reached.
struct ImmediateEffect {
  auto operator==(const ImmediateEffect&) const -> bool = default;
};

// The effect becomes a nonblocking update event, due in the NBA region of the
// slot `control` names -- this one, where the source wrote none. Everything the
// effect needs is read where the statement is reached, and the procedure
// carries on without waiting for it (LRM 4.4.2.4, 9.4.5, 10.4.2, 15.5.1).
struct NonBlockingEffect {
  std::optional<DelayOrEventControl> control = std::nullopt;

  auto operator==(const NonBlockingEffect&) const -> bool = default;
};

// When an effect the source wrote as one statement actually happens. A
// procedural assignment and an event trigger each spell both alternatives, and
// the standard gives the second the same meaning in both.
using EffectTiming = std::variant<ImmediateEffect, NonBlockingEffect>;

}  // namespace lyra::hir
