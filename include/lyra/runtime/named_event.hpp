#pragma once

#include <optional>

#include "lyra/base/time.hpp"
#include "lyra/runtime/event.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// Parks the calling frame on the event's waiter set. The engine never sees a
// "waiting for an event" state -- the coroutine simply suspends, and the
// producer schedules it again on trigger through the same construct-neutral
// verb every other wait uses, so the engine has no event-specific code path.
class EventAwaitable : public PendingWait {
 public:
  explicit EventAwaitable(RuntimeEvent& event) : event_(&event) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    CoroutineHandle token = &handle.promise();
    event_->AddWaiter(token);
    token->process->BlockLeaf(token, this);
  }

  static void await_resume() noexcept {
  }

  // A named-event trigger is instantaneous (LRM 15.5): a trigger during
  // suspension is missed, so resume re-subscribes for the next one. No engine
  // runtime access is needed; the capability signature carries it uniformly.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeEffects&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    event_->AddWaiter(activation);
    return PendingWaitOutcome::kReblocked;
  }

 private:
  RuntimeEvent* event_;
};

// SystemVerilog named event (LRM 15.5). A field of this type lives on the
// module state struct; it owns the waiters list and a timestamp recording
// when it was last triggered. The "triggered in current time step"
// semantic (LRM 15.5.3) is realised by comparing `last_triggered_at_`
// against `runtime.Now()` -- there is no slot bookkeeping in the event
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
  void Trigger(RuntimeEffects& runtime) {
    last_triggered_at_ = runtime.Now();
    for (CoroutineHandle waiter : event_.TakeWaiters()) {
      runtime.ScheduleNextDelta(waiter);
    }
  }

  // LRM 15.5.2: `@e;` blocks until the next trigger.
  auto Await() -> EventAwaitable {
    return EventAwaitable{event_};
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
  RuntimeEvent event_;
  std::optional<SimTime> last_triggered_at_;
};

}  // namespace lyra::runtime
