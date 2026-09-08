#pragma once

#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/event_edge.hpp"

namespace lyra::hir {

struct DelayControl {
  ExprId duration;
};

// One leaf entry of a wait's read set. Identity-only: which cell, and the
// flat-bit footprint of its packed encoding the leaf reads. An absent footprint
// means the whole signal is read.
struct SensitivityEntry {
  ValueTarget ref;
  std::optional<std::pair<std::uint64_t, std::uint64_t>> footprint;
};

// One entry of an explicit `@(...)` event control (LRM 9.4.2). `signal` is the
// expression the event is a change in the value of, `edge` the direction its
// least significant bit must take where one was written, and
// `sensitivity_list` the variables that expression reads -- a change to one of
// which is a candidacy for the event rather than the event itself.
//
// `condition` is the LRM 9.4.2.3 `iff` qualifier: a value change is an event
// only while it holds. It is read where the change happens, and what it reads
// is no part of what the entry is sensitive to, because the standard evaluates
// it when the watched expression moves and not when the qualifier itself does.
struct EventTrigger {
  ExprId signal;
  support::EventEdge edge;
  std::vector<SensitivityEntry> sensitivity_list;
  std::optional<ExprId> condition;
};

struct EventControl {
  std::vector<EventTrigger> triggers;
};

// LRM 9.4.2.2 `@*` / `@(*)`. Sensitivity for the controlled body is
// computed by slang's AnalysisManager (write-before-read exclusion via
// must-def) and looked up at AST -> HIR via the precomputed read-set facts.
struct ImplicitEventControl {
  std::vector<SensitivityEntry> sensitivity_list;
};

// LRM 15.5.2 `@e;`. What the wait watches is the event rather than the value of
// an expression, so the entry names storage and nothing else -- a trigger is
// the event itself, and there is no value to have moved. `condition` is the LRM
// 9.4.2.3 `iff` qualifier, read where the trigger happens.
struct NamedEventControl {
  SensitivityEntry event;
  std::optional<ExprId> condition;
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
struct ImmediateEffect {};

// The effect becomes a nonblocking update event, due in the NBA region of the
// slot `control` names -- this one, where the source wrote none. Everything the
// effect needs is read where the statement is reached, and the procedure
// carries on without waiting for it (LRM 4.4.2.4, 9.4.5, 10.4.2, 15.5.1).
struct NonBlockingEffect {
  std::optional<DelayOrEventControl> control = std::nullopt;
};

// When an effect the source wrote as one statement actually happens. A
// procedural assignment and an event trigger each spell both alternatives, and
// the standard gives the second the same meaning in both.
using EffectTiming = std::variant<ImmediateEffect, NonBlockingEffect>;

}  // namespace lyra::hir
