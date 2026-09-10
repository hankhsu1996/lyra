#pragma once

#include <concepts>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/event_edge.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

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
  template <std::invocable Evaluate>
  ValueWatch(Evaluate evaluate, support::EventEdge edge)
      : evaluate_([evaluate = std::move(evaluate)]() -> value::RuntimeValue {
          return value::RuntimeValue{evaluate()};
        }),
        edge_(edge) {
    Arm();
  }

  // Takes the expression's current value as the baseline every later comparison
  // is against.
  void Arm() {
    baseline_ = evaluate_();
  }

  // Whether the expression moved the way this control asks for, advancing the
  // baseline either way. An edge reads only the expression's least significant
  // bit; any other event is a change anywhere in its value (LRM 9.4.2).
  [[nodiscard]] auto TakeTransition() -> bool {
    value::RuntimeValue current = evaluate_();
    const bool moved =
        edge_ == support::EventEdge::kAnyChange
            ? !value::RuntimeValueBitIdentical(baseline_, current)
            : EdgeMatches(edge_, ClassifyEdge(Lsb(baseline_), Lsb(current)));
    baseline_ = std::move(current);
    return moved;
  }

 private:
  // An edge is a transition of one bit, so the expression it watches is a
  // packed value; the front end admits no other operand under an edge
  // specifier, and one reaching here is a lowering that let it through.
  [[nodiscard]] static auto Lsb(const value::RuntimeValue& value)
      -> value::FourStateBit {
    const auto* packed = std::get_if<value::PackedArray>(&value.value);
    if (packed == nullptr) {
      throw InternalError(
          "ValueWatch: an edge event control watches a value with no bits");
    }
    return packed->Lsb();
  }

  std::function<value::RuntimeValue()> evaluate_;
  value::RuntimeValue baseline_;
  support::EventEdge edge_ = support::EventEdge::kAnyChange;
};

// What decides whether reaching a wait is an event for it, held while the
// procedure waits there. Two halves, each present exactly where the source put
// one: an event control watches an expression's value, and a named event's
// trigger is the event itself so it watches nothing; either may carry an `iff`
// qualifier, which is read where the change happens and not when the qualifier
// itself moves (LRM 9.4.2, 9.4.2.3, 15.5).
//
// Both live no longer than the wait. A procedure that is not waiting at an
// event control has no observation there, so a change while it is elsewhere is
// not detected -- which is what the standard requires of a procedure that has
// left and re-reached the control.
class ArmedObservation {
 public:
  // Built where execution reaches the event control, which is where the wait
  // begins, so it is armed from the outset. `edge` arrives as a PackedArray
  // literal, the way every compile-time scalar crosses into a runtime entry.
  template <std::invocable Evaluate>
  ArmedObservation(Evaluate evaluate, const value::PackedArray& edge)
      : watch_(std::in_place, std::move(evaluate), EdgeOf(edge)) {
  }

  template <std::invocable Evaluate, std::invocable Condition>
  ArmedObservation(
      Evaluate evaluate, const value::PackedArray& edge, Condition condition)
      : watch_(std::in_place, std::move(evaluate), EdgeOf(edge)),
        condition_(WrapCondition(std::move(condition))) {
  }

  // The qualifier alone, for a wait whose target decides by being reached: a
  // named event's trigger is the event, and `iff` is the whole of what can
  // still hold it back.
  template <std::invocable Condition>
  explicit ArmedObservation(Condition condition)
      : condition_(WrapCondition(std::move(condition))) {
  }

  // Re-takes the baseline for a wait that is being established again.
  void Arm() {
    if (watch_.has_value()) {
      watch_->Arm();
    }
  }

  // Whether the change that made this a candidate is an event for it.
  //
  // The watched expression is read first and unconditionally, because the
  // qualifier gates the event and not the watching: a change it holds back is
  // still a change, and the baseline has to advance to it or the wait goes on
  // comparing against a value the design has left behind.
  [[nodiscard]] auto Fires() -> bool {
    if (watch_.has_value() && !watch_->TakeTransition()) {
      return false;
    }
    return !condition_ || condition_();
  }

 private:
  [[nodiscard]] static auto EdgeOf(const value::PackedArray& edge)
      -> support::EventEdge {
    return static_cast<support::EventEdge>(edge.ToInt64());
  }

  // The qualifier arrives already reduced to LRM 12.4 truth as a one-bit value,
  // because that reduction is the language's and belongs where the expression
  // is compiled rather than here.
  template <std::invocable Condition>
  [[nodiscard]] static auto WrapCondition(Condition condition)
      -> std::function<bool()> {
    return [condition = std::move(condition)]() -> bool {
      const value::RuntimeValue held{condition()};
      const auto* packed = std::get_if<value::PackedArray>(&held.value);
      if (packed == nullptr) {
        throw InternalError(
            "ArmedObservation: an `iff` qualifier answers a value with no "
            "bits");
      }
      return packed->IsTruthy();
    };
  }

  std::optional<ValueWatch> watch_;
  std::function<bool()> condition_;
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
class Observation {
 public:
  Observation() = default;

  // Being reached is the whole condition, so there is nothing armed to hold.
  [[nodiscard]] static auto OnReaching() -> Observation {
    return Observation{};
  }

  template <std::invocable Evaluate>
  [[nodiscard]] static auto OfValue(
      Evaluate evaluate, const value::PackedArray& edge) -> Observation {
    return Observation{
        std::make_shared<ArmedObservation>(std::move(evaluate), edge)};
  }

  template <std::invocable Evaluate, std::invocable Condition>
  [[nodiscard]] static auto OfValueQualified(
      Evaluate evaluate, const value::PackedArray& edge, Condition condition)
      -> Observation {
    return Observation{std::make_shared<ArmedObservation>(
        std::move(evaluate), edge, std::move(condition))};
  }

  template <std::invocable Condition>
  [[nodiscard]] static auto Qualified(Condition condition) -> Observation {
    return Observation{
        std::make_shared<ArmedObservation>(std::move(condition))};
  }

  [[nodiscard]] auto Get() const -> ArmedObservation* {
    return held_.get();
  }

 private:
  explicit Observation(std::shared_ptr<ArmedObservation> held)
      : held_(std::move(held)) {
  }

  std::shared_ptr<ArmedObservation> held_;
};

}  // namespace lyra::runtime
