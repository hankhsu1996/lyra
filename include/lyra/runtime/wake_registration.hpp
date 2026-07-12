#pragma once

namespace lyra::runtime {

struct PromiseBase;

// A revocable, one-shot link between a wake source -- a fork's join state, a
// process's `wait fork` condition -- and the single frame parked on it. At most
// one frame parks per source: a fork has one forking process, and a `wait fork`
// condition has the one process that executed it.
//
// Either end may be torn down first. Arming links the two; a wake or a revoke
// unlinks both, so whichever happens first wins and the other becomes a no-op.
// A parked frame is therefore never resumed into freed storage, and a source
// never wakes a frame that has been destroyed. LRM 9.6 process control destroys
// an activation while it is parked, which is what makes the revoke direction
// reachable.
class WakeRegistration {
 public:
  WakeRegistration() = default;
  WakeRegistration(const WakeRegistration&) = delete;
  auto operator=(const WakeRegistration&) -> WakeRegistration& = delete;
  // Non-movable: the parked frame holds a pointer back to this link.
  WakeRegistration(WakeRegistration&&) = delete;
  auto operator=(WakeRegistration&&) -> WakeRegistration& = delete;
  ~WakeRegistration();

  void Arm(PromiseBase* waiter);

  // The source's wake condition is met: unlink the parked frame and hand it
  // back for scheduling, or null if none is parked or it already detached.
  [[nodiscard]] auto TakeForWake() -> PromiseBase*;

  // `waiter`'s frame is being torn down or cancelled: drop it so a later wake
  // is a no-op.
  void RevokeWaiter(PromiseBase* waiter);

 private:
  PromiseBase* waiter_ = nullptr;
};

}  // namespace lyra::runtime
