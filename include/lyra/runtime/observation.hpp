#pragma once

#include <memory>
#include <optional>

#include "lyra/runtime/closure.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::runtime {

// What decides whether reaching a wait is an event for it, held while the
// procedure waits there. Two halves, each present exactly where the source put
// one: an event control watches an expression's value, and a named event's
// trigger is the event itself so it watches nothing; either may carry an `iff`
// qualifier, which is read when what is watched moves and not when the
// qualifier itself does (LRM 9.4.2, 9.4.2.3, 15.5).
//
// Both live no longer than the wait. A change while the procedure is not parked
// at the event control is not one it reports, and the control measures from
// what its expression is worth when the procedure reaches it again -- which is
// what the standard requires of a procedure that has left and re-reached the
// control.
//
// Whoever asks runs the expression and the qualifier, so asking is done either
// by the change -- for an evaluation that only reads storage, which nothing
// can tell from the waiting process running it (LRM 4.7) -- or by the waiting
// process, whose evaluation the change schedules (LRM 4.5); an evaluation that
// can call, write or fail is only ever the waiting process's.
//
// What is watched is the expression's watch, which the compiler builds for it:
// asked to arm, it takes what the expression is worth now as the baseline;
// asked otherwise, it answers whether the expression moved the way the control
// asks for -- a change anywhere in its value, or the edge its least significant
// bit makes -- and advances the baseline either way (LRM 9.4.2). A candidate
// that does not fire still advances it, so consecutive comparisons are between
// consecutive observed states. The comparison is the expression's type's own,
// compiled with it; nothing here knows the type.
class ArmedObservation {
 public:
  // Built unarmed: it has nothing to compare against until the expression is
  // first evaluated. A wait whose target decides by being reached -- a named
  // event's trigger is the event -- watches nothing, and `iff` is the whole of
  // what can still hold it back. The qualifier answers whether it holds, its
  // value already reduced to LRM 12.4 truth where the expression is compiled.
  ArmedObservation(
      std::optional<OwnedClosure<support::WatchSignature>> watch,
      std::optional<OwnedClosure<support::ConditionSignature>> condition);

  ArmedObservation(const ArmedObservation&) = delete;
  auto operator=(const ArmedObservation&) -> ArmedObservation& = delete;
  ArmedObservation(ArmedObservation&&) = delete;
  auto operator=(ArmedObservation&&) -> ArmedObservation& = delete;
  ~ArmedObservation();

  // Takes what the expression is worth now as what later changes are measured
  // from: where the wait begins, and again where a stopped process starts
  // waiting afresh (LRM 9.7).
  void Arm() {
    // Told it is arming, the watch only takes its baseline, so what it
    // answers then is no event.
    if (watch_.has_value()) {
      (*watch_)(true);
    }
    armed_ = true;
  }

  // Leaves the observation with nothing to compare against, for a wait whose
  // process re-arms it on its next evaluation rather than wherever the restart
  // is asked for.
  void Disarm() {
    armed_ = false;
  }

  // Whether the candidacy being asked about is an event for this wait. An
  // observation with nothing to compare against arms instead, which is no
  // event: a change is measured from a value the wait has seen.
  //
  // The watched expression is read first and unconditionally, because the
  // qualifier gates the event and not the watching: a change it holds back is
  // still a change, and the baseline has to advance to it or the wait goes on
  // comparing against a value the design has left behind.
  [[nodiscard]] auto Fires() -> bool {
    if (!armed_) {
      Arm();
      return false;
    }
    return (!watch_.has_value() || (*watch_)(false)) &&
           (!condition_.has_value() || (*condition_)());
  }

 private:
  std::optional<OwnedClosure<support::WatchSignature>> watch_;
  std::optional<OwnedClosure<support::ConditionSignature>> condition_;
  bool armed_ = false;
};

// What a wait carries an observation as. One event expression has one
// observation however many places watch for it, and it lives as long as the
// wait rather than as long as the statement that reached the control, so
// whatever is watching holds it between them and none of them owns it.
//
// One factory per form the language distinguishes. Watching nothing is
// the implicit sensitivity of an `always_comb` / `always_latch` body, an `@*`,
// a `wait (cond)` or a continuous assignment -- the standard makes those
// sensitive to the variables read rather than to the value of an expression
// (LRM 9.2.2.2.1), so being reached is the whole condition -- and is equally an
// unqualified `@e`, whose trigger is the event itself (LRM 15.5.1).
//
// Every member no expression shapes is defined in the library: a unit stating
// a wait constructs, copies and destroys these, and a definition written here
// would be compiled again by each such unit.
class Observation {
 public:
  Observation();
  Observation(const Observation&);
  auto operator=(const Observation&) -> Observation&;
  Observation(Observation&&) noexcept;
  auto operator=(Observation&&) noexcept -> Observation&;
  ~Observation();

  // Being reached is the whole condition, so there is nothing armed to hold.
  [[nodiscard]] static auto OnReaching() -> Observation;

  // An expression's watch, with or without an `iff` qualifier, and the
  // qualifier alone, which answers whether it holds as LRM 12.4 decides.
  [[nodiscard]] static auto OfWatch(OwnedClosure<support::WatchSignature> watch)
      -> Observation;
  [[nodiscard]] static auto OfWatchQualified(
      OwnedClosure<support::WatchSignature> watch,
      OwnedClosure<support::ConditionSignature> condition) -> Observation;
  [[nodiscard]] static auto Qualified(
      OwnedClosure<support::ConditionSignature> condition) -> Observation;

  // Arms what this holds, where the wait begins. Being reached holds nothing to
  // arm.
  void Arm() const;

  // Leaves what this holds with nothing to compare against, for the waiting
  // process to arm on its next evaluation.
  void Disarm() const;

  // Whether a candidacy is an event for the wait: asked by the change where
  // the evaluation only reads storage (LRM 4.7), and by the waiting process
  // where the evaluation is its own (LRM 4.5). Being reached is the whole
  // condition where nothing is held.
  [[nodiscard]] auto Fires() const -> bool;

 private:
  explicit Observation(std::shared_ptr<ArmedObservation> held);

  std::shared_ptr<ArmedObservation> held_;
};

}  // namespace lyra::runtime
