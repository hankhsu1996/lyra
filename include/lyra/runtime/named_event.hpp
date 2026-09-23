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
//
// Every member is defined in this class's own source file, the constructor and
// destructor included: a unit holding an event constructs and destroys it, and
// a definition written here would be compiled again by each such unit.
class NamedEvent : public Observable {
 public:
  NamedEvent();

  // Non-movable: waiting here takes pointers into the waiter set, so the
  // address must be stable once a process has waited.
  NamedEvent(const NamedEvent&) = delete;
  auto operator=(const NamedEvent&) -> NamedEvent& = delete;
  NamedEvent(NamedEvent&&) = delete;
  auto operator=(NamedEvent&&) -> NamedEvent& = delete;
  ~NamedEvent();

  // LRM 15.5.1: `-> e;` records the time it fired and ends the wait of every
  // process the trigger is an event for.
  void Trigger(RuntimeEffects& runtime);

  // LRM 15.5.3: `e.triggered` is true iff the most recent trigger happened
  // in the current simulation time step. No mutation, no clearing -- the
  // answer is a timestamp comparison.
  [[nodiscard]] auto Triggered(RuntimeEffects& runtime) const
      -> value::PackedArray;

 private:
  std::optional<SimTime> last_triggered_at_;
};

}  // namespace lyra::runtime
