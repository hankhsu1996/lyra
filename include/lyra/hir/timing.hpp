#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/event_edge.hpp"

namespace lyra::hir {

struct DelayControl {
  ExprId duration;

  auto operator==(const DelayControl&) const -> bool = default;
};

// One cell of a read set, a cell elaboration sealed, and the flat-bit footprint
// of its packed encoding the read reaches. An absent footprint means the whole
// of it is read.
struct SensitivityEntry {
  ValueTarget cell;
  std::optional<std::pair<std::uint64_t, std::uint64_t>> footprint;

  auto operator==(const SensitivityEntry&) const -> bool = default;
};

// The object the running method was called on, which a property named bare
// inside the method belongs to (LRM 8.4). It is no handle: a method runs on an
// object, so there is nothing to test before reaching it.
struct ReceiverObject {
  auto operator==(const ReceiverObject&) const -> bool = default;
};

// The objects a chain of property reads passes through (LRM 8.4): the one the
// root is -- the object a class handle names, or the running method's own --
// then the one each hop's handle-valued property holds. Each is watched as one
// source covering every property it has, so a change to any of them
// reevaluates the expression (LRM 9.4.2). Which objects those are is known only
// by evaluating the chain, each time the wait collects its leaves, and a null
// link ends it there, having reached nothing further.
struct ObjectChain {
  // A handle-valued property of the object before it, and the type of the
  // handle it holds.
  struct Hop {
    ClassPropertyTarget property;
    TypeId handle_type;

    auto operator==(const Hop&) const -> bool = default;
  };

  std::variant<ExprId, ReceiverObject> root;
  std::vector<Hop> hops;

  auto operator==(const ObjectChain&) const -> bool = default;
};

// Every object at once. A read that reaches an object along no chain a report
// can evaluate -- through a variable of a subroutine's own, or a handle it was
// handed from one -- is covered by reevaluating whenever a property of any
// object changes, which LRM 9.4.2 permits for members the expression does not
// read.
struct EveryObject {
  auto operator==(const EveryObject&) const -> bool = default;
};

// What a wait watches: a cell elaboration sealed; a variable of the instance a
// virtual interface holds; the objects a chain of reads passes through; or
// every object. All but the first are found each time the wait collects its
// leaves.
using WaitLeaf = std::variant<
    SensitivityEntry, InterfaceMemberAccessExpr, ObjectChain, EveryObject>;

// How a call a report makes passes one argument: as the call site evaluates
// it, or as its type's default where evaluating it there could read what the
// report is not standing in -- a variable of the subroutine's own, a handle it
// would dereference, or another call.
enum class ReportedArgument : std::uint8_t { kEvaluated, kDefaulted };

// A call made to learn what it reads: the call as the source wrote it, how the
// object it is made on is passed where it is made on one, and how each of its
// arguments is.
struct ReportingCall {
  ExprId call;
  std::optional<ReportedArgument> receiver;
  std::vector<ReportedArgument> arguments;

  auto operator==(const ReportingCall&) const -> bool = default;
};

// What an evaluation can read (LRM 9.4.2): the leaves it reaches itself, and
// the calls of subroutines it makes, each of which reports what it reads in
// turn. A wait on an expression watches the whole of it, and a function body
// states it for whichever wait calls the function.
//
// `unreportable` says why a report cannot be made, where the evaluation reads
// something no leaf yet watches. A function body is compiled whether or not a
// wait ever calls it, so that is not an error of the body's; it is raised when
// a wait asks.
struct Reads {
  std::vector<WaitLeaf> leaves;
  std::vector<ReportingCall> calls;
  std::optional<std::string> unreportable;

  auto operator==(const Reads&) const -> bool = default;
};

// One entry of an explicit `@(...)` event control (LRM 9.4.2). `signal` is the
// expression the event is a change in the value of, `edge` the direction its
// least significant bit must take where one was written, and `reads` what that
// expression reads -- a change to any of which is a candidacy for the event
// rather than the event itself.
//
// `condition` is the LRM 9.4.2.3 `iff` qualifier: a value change is an event
// only while it holds. It is read where the change happens, and what it reads
// is no part of what the entry is sensitive to, because the standard evaluates
// it when the watched expression moves and not when the qualifier itself does.
struct EventTrigger {
  ExprId signal;
  support::EventEdge edge{};
  Reads reads;
  std::optional<ExprId> condition;

  auto operator==(const EventTrigger&) const -> bool = default;
};

struct EventControl {
  std::vector<EventTrigger> triggers;

  auto operator==(const EventControl&) const -> bool = default;
};

// LRM 9.4.2.2 `@*` / `@(*)`. Sensitivity for the controlled body is
// computed by slang's AnalysisManager (write-before-read exclusion via
// must-def) and looked up at AST -> HIR via the precomputed read-set facts.
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
