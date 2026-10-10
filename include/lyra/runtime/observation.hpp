#pragma once

#include <concepts>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/event_edge.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

// How one value change reads as a direction, per LRM 9.4.2 Table 9-2: leaving 0
// is a posedge and leaving 1 a negedge whatever the destination, and from x or
// z only arrival at 1 or 0 names one, so x <-> z names none.
enum class EdgeTransition : std::uint8_t {
  kChangeOnly,
  kPosedge,
  kNegedge,
};

inline auto ClassifyEdge(
    value::FourStateBit old_lsb, value::FourStateBit new_lsb)
    -> EdgeTransition {
  if (old_lsb == new_lsb) {
    return EdgeTransition::kChangeOnly;
  }
  if (old_lsb == value::FourStateBit::kZero) {
    return EdgeTransition::kPosedge;
  }
  if (old_lsb == value::FourStateBit::kOne) {
    return EdgeTransition::kNegedge;
  }
  if (new_lsb == value::FourStateBit::kOne) {
    return EdgeTransition::kPosedge;
  }
  if (new_lsb == value::FourStateBit::kZero) {
    return EdgeTransition::kNegedge;
  }
  return EdgeTransition::kChangeOnly;
}

// Whether the transition names a direction, as opposed to a change that names
// none (LRM 9.4.2 Table 9-2).
[[nodiscard]] constexpr auto IsDirectedEdge(EdgeTransition transition) -> bool {
  switch (transition) {
    case EdgeTransition::kPosedge:
    case EdgeTransition::kNegedge:
      return true;
    case EdgeTransition::kChangeOnly:
      return false;
  }
  throw InternalError("runtime::IsDirectedEdge: unknown EdgeTransition");
}

inline auto EdgeMatches(support::EventEdge edge, EdgeTransition transition)
    -> bool {
  switch (edge) {
    case support::EventEdge::kAnyChange:
      return true;
    case support::EventEdge::kPosedge:
      return transition == EdgeTransition::kPosedge;
    case support::EventEdge::kNegedge:
      return transition == EdgeTransition::kNegedge;
    case support::EventEdge::kBothEdges:
      return IsDirectedEdge(transition);
  }
  throw InternalError("runtime::EdgeMatches: unknown EventEdge");
}

// Which edge an event control names, as it crosses into a runtime entry: a
// machine integer, the way every runtime scalar does.
[[nodiscard]] auto EventEdgeOf(std::int64_t edge) -> support::EventEdge;

// Whether a bit going from `before` to `now` is the edge `edge` names. Every
// unit stating an edge control reaches it, and no design shapes it, so it is
// compiled once, in the library.
[[nodiscard]] auto IsEdge(
    support::EventEdge edge, value::FourStateBit before,
    value::FourStateBit now) -> bool;

// The expression an event control is watching, and what it was worth when the
// wait began (LRM 9.4.2).
//
// A change to a variable the expression reads only makes the wait a candidate.
// The event itself is a change in the value of the *expression*, so an operand
// that moves without moving the result is no event -- which is answerable only
// here, because only the wait knows what it is watching. A candidate that does
// not fire advances the baseline, so consecutive comparisons are between
// consecutive observed states: watching for a posedge while the operand sits at
// 1, a fall to 0 does not fire, and without advancing, the rise back to 1 would
// compare 1 against 1 and miss the edge.
class ValueWatch {
 public:
  ValueWatch(const ValueWatch&) = delete;
  auto operator=(const ValueWatch&) -> ValueWatch& = delete;
  ValueWatch(ValueWatch&&) = delete;
  auto operator=(ValueWatch&&) -> ValueWatch& = delete;
  virtual ~ValueWatch();

  // Takes the expression's current value as the baseline every later comparison
  // is against.
  virtual void Arm() = 0;

  // Whether the expression moved the way this control asks for, advancing the
  // baseline either way. An edge reads only the expression's least significant
  // bit; any other event is a change anywhere in its value (LRM 9.4.2).
  [[nodiscard]] virtual auto TakeTransition() -> bool = 0;

 protected:
  ValueWatch();
};

// A watch over an expression whose value is whatever `Evaluate` answers, which
// it compares as that value's own type does.
//
// What a unit stating an event control reaches is building and destroying the
// watch, which the expression shapes; what only the engine reaches, when a
// change asks whether it was an event, is called through the watch it holds.
template <std::invocable Evaluate>
class ValueWatchOf final : public ValueWatch {
 public:
  // Building the watch evaluates nothing: an evaluation belongs to whoever
  // arms or asks it, which is what decides the process it runs in.
  ValueWatchOf(Evaluate evaluate, support::EventEdge edge)
      : evaluate_(std::move(evaluate)), edge_(edge) {
  }

  void Arm() override {
    baseline_.emplace(evaluate_());
  }

  [[nodiscard]] auto TakeTransition() -> bool override {
    Value current = evaluate_();
    const bool moved = Moved(*baseline_, current);
    baseline_ = std::move(current);
    return moved;
  }

 private:
  using Value = std::invoke_result_t<Evaluate&>;

  // An edge is a transition of one bit, so the expression it watches is a
  // packed value; the front end admits no other operand under an edge
  // specifier, and one reaching here is a lowering that let it through.
  [[nodiscard]] auto Moved(const Value& before, const Value& now) const
      -> bool {
    if (edge_ == support::EventEdge::kAnyChange) {
      return !before.IsBitIdentical(now);
    }
    if constexpr (requires {
                    { before.Lsb() } -> std::same_as<value::FourStateBit>;
                  }) {
      return IsEdge(edge_, before.Lsb(), now.Lsb());
    } else {
      throw InternalError(
          "ValueWatch: an edge event control watches a value with no bits");
    }
  }

  Evaluate evaluate_;
  std::optional<Value> baseline_;
  support::EventEdge edge_;
};

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
// As with a single watch, what a unit reaches is what the expression and the
// qualifier shape, and members defined in the library; what only the engine
// asks is written here.
class ArmedObservation {
 public:
  // Built unarmed: it has nothing to compare against until the expression is
  // first evaluated. A wait whose target decides by being reached -- a named
  // event's trigger is the event -- watches nothing, and `iff` is the whole of
  // what can still hold it back. The qualifier answers whether it holds, its
  // value already reduced to LRM 12.4 truth where the expression is compiled,
  // because that reduction is the language's and belongs there rather than
  // here.
  ArmedObservation(
      std::unique_ptr<ValueWatch> watch,
      std::move_only_function<bool()> condition);

  ArmedObservation(const ArmedObservation&) = delete;
  auto operator=(const ArmedObservation&) -> ArmedObservation& = delete;
  ArmedObservation(ArmedObservation&&) = delete;
  auto operator=(ArmedObservation&&) -> ArmedObservation& = delete;
  ~ArmedObservation();

  // Takes what the expression is worth now as what later changes are measured
  // from: where the wait begins, and again where a stopped process starts
  // waiting afresh (LRM 9.7).
  void Arm() {
    if (watch_ != nullptr) {
      watch_->Arm();
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
    return (watch_ == nullptr || watch_->TakeTransition()) &&
           (!condition_ || condition_());
  }

 private:
  std::unique_ptr<ValueWatch> watch_;
  std::move_only_function<bool()> condition_;
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

  template <std::invocable Evaluate>
  [[nodiscard]] static auto OfValue(Evaluate evaluate, std::int64_t edge)
      -> Observation {
    return Observation{std::make_shared<ArmedObservation>(
        Watching(std::move(evaluate), edge), nullptr)};
  }

  template <std::invocable Evaluate, std::invocable Condition>
  [[nodiscard]] static auto OfValueQualified(
      Evaluate evaluate, std::int64_t edge, Condition condition)
      -> Observation {
    return Observation{std::make_shared<ArmedObservation>(
        Watching(std::move(evaluate), edge), Holding(std::move(condition)))};
  }

  template <std::invocable Condition>
  [[nodiscard]] static auto Qualified(Condition condition) -> Observation {
    return Observation{std::make_shared<ArmedObservation>(
        nullptr, Holding(std::move(condition)))};
  }

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

  template <std::invocable Evaluate>
  [[nodiscard]] static auto Watching(Evaluate evaluate, std::int64_t edge)
      -> std::unique_ptr<ValueWatch> {
    return std::make_unique<ValueWatchOf<Evaluate>>(
        std::move(evaluate), EventEdgeOf(edge));
  }

  // Whether the one-bit value `condition` answers holds (LRM 12.4).
  template <std::invocable Condition>
  [[nodiscard]] static auto Holding(Condition condition)
      -> std::move_only_function<bool()> {
    return [condition = std::move(condition)]() mutable {
      return condition().IsTruthy();
    };
  }

  std::shared_ptr<ArmedObservation> held_;
};

}  // namespace lyra::runtime
