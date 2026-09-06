#pragma once

#include <concepts>
#include <cstdint>
#include <functional>
#include <memory>
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

// What an event control holds while a procedure waits at it (LRM 9.4.2): the
// expression whose value decides the event, and the value that expression had
// when the wait began.
//
// A change to a variable the expression reads only makes this a candidate. The
// event itself is a change in the value of the *expression*, so an operand that
// moves without moving the result is no event -- which is answerable only here,
// because only the wait knows what it is watching. A candidate that does not
// fire advances the baseline, so consecutive comparisons are between
// consecutive observed states: watching for a posedge while the operand sits at
// 1, a fall to 0 does not fire, and without advancing, the rise back to 1 would
// compare 1 against 1 and miss the edge.
//
// The baseline lives no longer than the wait. A procedure that is not waiting
// at an event control has no observation there, so a change while it is
// elsewhere is not detected -- which is what the standard requires of a
// procedure that has left and re-reached the control.
class ArmedObservation {
 public:
  // Built where execution reaches the event control, which is where the wait
  // begins, so it is armed from the outset. `edge` arrives as a PackedArray
  // literal, the way every compile-time scalar crosses into a runtime entry.
  template <std::invocable Evaluate>
  ArmedObservation(Evaluate evaluate, const value::PackedArray& edge)
      : evaluate_([evaluate = std::move(evaluate)]() -> value::RuntimeValue {
          return value::RuntimeValue{evaluate()};
        }),
        edge_(static_cast<support::EventEdge>(edge.ToInt64())) {
    Arm();
  }

  // Takes the expression's current value as the baseline every later comparison
  // is against.
  void Arm() {
    baseline_ = evaluate_();
  }

  // Whether the change that made this a candidate is an event for it. An edge
  // reads only the expression's least significant bit; any other event is a
  // change anywhere in its value (LRM 9.4.2).
  [[nodiscard]] auto Fires() -> bool {
    value::RuntimeValue current = evaluate_();
    const bool fires =
        edge_ == support::EventEdge::kAnyChange
            ? !value::RuntimeValueBitIdentical(baseline_, current)
            : EdgeMatches(edge_, ClassifyEdge(Lsb(baseline_), Lsb(current)));
    baseline_ = std::move(current);
    return fires;
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
          "ArmedObservation: an edge event control watches a value with no "
          "bits");
    }
    return packed->Lsb();
  }

  std::function<value::RuntimeValue()> evaluate_;
  value::RuntimeValue baseline_;
  support::EventEdge edge_ = support::EventEdge::kAnyChange;
};

// What a wait carries an observation as. Every leaf of one event expression
// names the same observation, and the wait outlives the statement that reached
// the control, so the leaves hold it between them rather than any one of them
// or the procedure's frame owning it.
//
// A default-built handle names none, which is the implicit sensitivity of an
// `always_comb` / `always_latch` body, an `@*`, a `wait (cond)` or a continuous
// assignment: the standard makes those sensitive to the variables read rather
// than to the value of an expression (LRM 9.2.2.2.1), so being reached is the
// whole condition.
class Observation {
 public:
  Observation() = default;

  template <std::invocable Evaluate>
  Observation(Evaluate evaluate, const value::PackedArray& edge)
      : held_(std::make_shared<ArmedObservation>(std::move(evaluate), edge)) {
  }

  [[nodiscard]] auto Get() const -> ArmedObservation* {
    return held_.get();
  }

 private:
  std::shared_ptr<ArmedObservation> held_;
};

}  // namespace lyra::runtime
