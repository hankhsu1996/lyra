#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/region.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class Timer;

// A timer's place in the queue of the region it is due in, as a node of that
// queue's list.
struct TimerPlace : IntrusiveListNode<TimerPlace> {
  // Null on the sentinel a list embeds to close its ring.
  Timer* timer = nullptr;
};

// One future event at most, which arming again replaces: what a continuous
// assignment carrying a delay propagates through (LRM 10.3.3), and what an
// expect statement's wait hands each tick of its property to the Observed
// region by (LRM 16.17). A right-hand side that changes before its previous
// value has propagated deschedules that propagation, so the event already
// scheduled is taken off its queue and the new one queued in its place; a slot
// left holding nothing is then passed over as one whose every event was taken
// back is (LRM 4.5).
//
// The timer is the queue node, held by whatever owns the event -- the scope
// that owns the assignment, the wait an expect statement stops at -- so arming
// it costs no allocation and a pending event needs no record of its own. What
// it does when it fires is bound once and reads its owner, which holds the
// value of the one propagation pending -- every new value arms the timer again
// -- so nothing is captured per change.
class Timer {
 public:
  Timer();
  Timer(const Timer&) = delete;
  auto operator=(const Timer&) -> Timer& = delete;
  Timer(Timer&&) = delete;
  auto operator=(Timer&&) -> Timer& = delete;
  ~Timer();

  // What firing does.
  void Bind(OwnedCall fire);

  // Schedules the event `ticks` steps from now, where a delay of that many
  // steps is due, in place of the one pending. A delay is counted in the steps
  // of the scope that writes it, whose precision is `precision_power`.
  void Arm(
      RuntimeEffects& effects, SimDuration ticks, std::int64_t precision_power);

  // Schedules the event in `region` of the current time step, in place of the
  // one pending.
  void ArmNow(RuntimeEffects& effects, Region region);

  // The event has come: it is no longer pending, and what it does is done.
  void Fire();

  [[nodiscard]] auto Place() -> TimerPlace& {
    return place_;
  }

 private:
  TimerPlace place_;
  OwnedCall fire_;
};

}  // namespace lyra::runtime
