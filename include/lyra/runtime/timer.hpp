#pragma once

#include <cstdint>
#include <memory>

#include "lyra/base/time.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/support/region.hpp"

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
// assignment carrying a delay propagates through (LRM 10.3.3), what an expect
// statement's wait hands each tick of its property to the Observed region by
// (LRM 16.17), and what calls foreign code back at a point of a time slot
// (LRM 38.36.2). A right-hand side that changes before its previous
// value has propagated deschedules that propagation, so the event already
// scheduled is taken off its queue and the new one queued in its place; a slot
// left holding nothing is then passed over as one whose every event was taken
// back is (LRM 4.5).
//
// The timer is the queue node, held by whatever owns the event -- the scope
// that owns the assignment, the wait an expect statement stops at, or shared
// by a callback and whoever fires it -- so arming
// it costs no allocation and a pending event needs no record of its own. What
// it does when it fires is bound once and reads its owner, which holds the
// value of the one propagation pending -- every new value arms the timer again
// -- so nothing is captured per change.
class Timer : public std::enable_shared_from_this<Timer> {
 public:
  Timer();
  Timer(const Timer&) = delete;
  auto operator=(const Timer&) -> Timer& = delete;
  Timer(Timer&&) = delete;
  auto operator=(Timer&&) -> Timer& = delete;
  ~Timer();

  // What firing does, bound by the procedure the timer works for: whoever owns
  // it binds it, where arming it can come from whatever moved what its owner
  // watches.
  void Bind(OwnedCall fire);

  // Schedules the event `ticks` steps from now, where a delay of that many
  // steps is due, in place of the one pending. A delay is counted in the steps
  // of the scope that writes it, whose precision is `precision_power`.
  void Arm(
      RuntimeEffects& effects, SimDuration ticks, std::int64_t precision_power);

  // Schedules the event in `region` of the current time step, in place of the
  // one pending.
  void ArmNow(RuntimeEffects& effects, support::Region region);

  // The event has come: it is no longer pending, and what it does is done.
  void Fire();

  // The procedure that bound it, which what it does reports for, no process
  // running then (LRM 20.10).
  [[nodiscard]] auto BoundBy() const -> const ProcedureContext& {
    return bound_by_;
  }

  [[nodiscard]] auto Place() -> TimerPlace& {
    return place_;
  }

  // What keeps it while it fires: a timer its owners share is kept by whoever
  // fires it, since what it does may let its last owner go; none for a timer
  // held by one owner, which outlives what it does.
  [[nodiscard]] auto KeepWhileFiring() -> std::shared_ptr<Timer> {
    return weak_from_this().lock();
  }

 private:
  TimerPlace place_;
  OwnedCall fire_;
  ProcedureContext bound_by_;
};

}  // namespace lyra::runtime
