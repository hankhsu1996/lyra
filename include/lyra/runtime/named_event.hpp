#pragma once

#include <optional>

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/event.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// Awaitable returned by `NamedEvent::Await()`. Subscribes the calling frame
// directly to the underlying `RuntimeEvent`'s waiter list. The engine never
// sees a "waiting for an event" state -- the coroutine simply suspends. The
// producer reaches back via `RuntimeServices::ScheduleNextDelta` when it
// triggers, so the engine has no event-specific code path.
class EventAwaitable {
 public:
  explicit EventAwaitable(RuntimeEvent& event) : event_(&event) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) noexcept {
    event_->AddWaiter(&handle.promise());
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeEvent* event_;
};

// SystemVerilog named event (LRM 15.5). A field of this type lives on the
// module state struct; it owns the waiters list and a timestamp recording
// when it was last triggered. The "triggered in current time step"
// semantic (LRM 15.5.3) is realised by comparing `last_triggered_at_`
// against `services.Now()` -- there is no slot bookkeeping in the event
// or in the engine.
class NamedEvent {
 public:
  NamedEvent() = default;

  // Non-movable: the runtime takes pointers into `event_` via Await(), so the
  // address must be stable once a process subscribes.
  NamedEvent(const NamedEvent&) = delete;
  auto operator=(const NamedEvent&) -> NamedEvent& = delete;
  NamedEvent(NamedEvent&&) = delete;
  auto operator=(NamedEvent&&) -> NamedEvent& = delete;
  ~NamedEvent() = default;

  // LRM 15.5.1: `-> e;` records the current simulation time and wakes every
  // currently-waiting process via the engine's generic scheduling primitive.
  void Trigger(RuntimeServices& services) {
    last_triggered_at_ = services.Now();
    for (CoroutineHandle waiter : event_.TakeWaiters()) {
      services.ScheduleNextDelta(waiter);
    }
  }

  // LRM 15.5.2: `@e;` blocks until the next trigger.
  auto Await() -> EventAwaitable {
    return EventAwaitable{event_};
  }

  // LRM 15.5.3: `e.triggered` is true iff the most recent trigger happened
  // in the current simulation time step. No mutation, no clearing -- the
  // answer is a timestamp comparison.
  [[nodiscard]] auto Triggered(RuntimeServices& services) const
      -> value::PackedArray {
    const bool hit =
        last_triggered_at_.has_value() && *last_triggered_at_ == services.Now();
    return value::PackedArray::FromInt(hit ? 1 : 0, 1, false, false);
  }

 private:
  RuntimeEvent event_;
  std::optional<SimTime> last_triggered_at_;
};

}  // namespace lyra::runtime
