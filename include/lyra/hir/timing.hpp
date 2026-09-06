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

// LRM 15.5.2 `@e;`. The controlled timing is a wait on a named event rather
// than a value-change event. HIR mirrors slang's TimingControl shape; HIR ->
// MIR collapses this onto a call against the named-event data type. The `event`
// ExprId resolves to a PrimaryExpr of a direct or routed reference pointing at
// the event variable. `condition` is the LRM 9.4.2.3 `iff` qualifier, read
// where the trigger happens.
struct NamedEventControl {
  ExprId event;
  std::optional<ExprId> condition;
};

using TimingControl = std::variant<
    DelayControl, EventControl, ImplicitEventControl, NamedEventControl>;

// An `event_control` in whichever of its two forms the source wrote (LRM A.6.5)
// -- a change in the value of an expression, or a named event's trigger. The
// name says "any" because `EventControl` alone is the value-change form.
using AnyEventControl = std::variant<EventControl, NamedEventControl>;

// LRM 9.4.5 `repeat (n) @(...)`: the update waits for that many occurrences of
// the event. The grammar puts a repeat count only in front of an event control,
// and only inside an assignment, so it is an intra-assignment control alone.
struct RepeatedEventControl {
  ExprId count;
  AnyEventControl event;
};

// LRM 9.4.5 `delay_or_event_control`: what an assignment may carry between its
// operator and its right-hand side. It is the timing controls a statement may
// be prefixed with, minus the implicit `@*` list -- which the grammar admits
// only in front of a statement -- plus the repeat form, which only an
// assignment admits.
using IntraAssignmentControl = std::variant<
    DelayControl, EventControl, NamedEventControl, RepeatedEventControl>;

}  // namespace lyra::hir
