#pragma once

#include <optional>

#include "lyra/base/time.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// SystemVerilog named event (LRM 15.5). A field of this type lives on the
// module state struct; it owns the waiters and a timestamp recording when it
// was last triggered. The "triggered in current time step" semantic (LRM
// 15.5.3) is realised by comparing that timestamp against the current time --
// there is no slot bookkeeping in the event or in the engine.
//
// A trigger carries no value, so nothing about it can be shown not to have
// reached a wait: every wait registered here is asked.
class NamedEvent : public Observable {
 public:
  NamedEvent() = default;

  // Non-movable: waiting here takes pointers into the waiter set, so the
  // address must be stable once a process has waited.
  NamedEvent(const NamedEvent&) = delete;
  auto operator=(const NamedEvent&) -> NamedEvent& = delete;
  NamedEvent(NamedEvent&&) = delete;
  auto operator=(NamedEvent&&) -> NamedEvent& = delete;
  ~NamedEvent() = default;

  // LRM 15.5.1: `-> e;` records the time it fired and ends the wait of every
  // process the trigger is an event for.
  void Trigger(RuntimeEffects& runtime) {
    last_triggered_at_ = runtime.Now();
    runtime.WakeWaitersOf(*this, MakeWholeValueProjectionTest());
  }

  // LRM 15.5.3: `e.triggered` is true iff the most recent trigger happened
  // in the current simulation time step. No mutation, no clearing -- the
  // answer is a timestamp comparison.
  [[nodiscard]] auto Triggered(RuntimeEffects& runtime) const
      -> value::PackedArray {
    const bool hit =
        last_triggered_at_.has_value() && *last_triggered_at_ == runtime.Now();
    return value::PackedArray::FromInt(hit ? 1 : 0, 1, false, false);
  }

 private:
  std::optional<SimTime> last_triggered_at_;
};

}  // namespace lyra::runtime
