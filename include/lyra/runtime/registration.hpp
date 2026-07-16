#pragma once

#include <cstdint>

#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

struct PromiseBase;

// The C++ realization of the payload-neutral activation token: a handle to an
// activation's execution core, naming it without naming the value it completes
// with. The scheduler holds this and nothing else. That it is a `PromiseBase*`
// today is the C++ backend's realization; a future LIR/LLVM backend realizes
// the same token concept differently, so no layer above the runtime names this
// type.
using CoroutineHandle = PromiseBase*;

// One membership: a target -- a value-change observable, a named event, a join
// condition, a scheduler region queue, a delay slot -- currently names one
// activation and can make it runnable.
//
// The membership is this one object. The activation owns it; the target only
// links it. So the relation is recorded once and reached from both ends, and
// revoking is an unlink: neither side ever searches the other, and neither can
// hold a stale copy of what the other believes. Releasing an activation
// destroys its registrations, which unlink themselves, so nothing is left able
// to resume it -- LRM 9.6 process control releases an activation while it is
// parked, which is what makes the revoke direction reachable.
//
// A registration settles exactly once: a target claims it when the wait is
// satisfied, or the activation revokes it. Either way it is unlinked, and an
// unlinked registration can resume nothing.
struct Registration {
  // Null on the sentinel a list embeds to close its ring, which is the one
  // Registration that names no activation.
  CoroutineHandle activation = nullptr;

  Registration* prev = nullptr;
  Registration* next = nullptr;

  // The fire condition of a value-change wait (LRM 9.4.2): which edge, and
  // which bit projection of the observable it watches. A membership that
  // carries no condition -- an event, a join, a scheduler queue -- leaves these
  // unset.
  support::EventEdge edge = support::EventEdge::kAnyChange;
  std::uint64_t lsb_bit_offset = 0;
  std::uint64_t bit_width = 0;

  Registration() = default;
  Registration(const Registration&) = delete;
  auto operator=(const Registration&) -> Registration& = delete;
  // Non-movable: a target's list points at this address.
  Registration(Registration&&) = delete;
  auto operator=(Registration&&) -> Registration& = delete;
  ~Registration() {
    Unlink();
  }

  // Detaches from whichever target holds it. Pointer surgery only -- which is
  // why a registration needs no pointer back to its target, and why a target is
  // never consulted to revoke one.
  void Unlink() noexcept {
    if (next == nullptr) {
      return;
    }
    prev->next = next;
    next->prev = prev;
    prev = nullptr;
    next = nullptr;
  }
};

// The registrations one target currently holds, threaded through nodes the
// activations own. The ring closes through an embedded sentinel, so an unlink
// touches no list state -- that is what keeps it constant-time and
// target-agnostic -- and moving a whole list onto another is a constant-time
// relink rather than a walk.
class RegistrationList {
 public:
  RegistrationList() {
    sentinel_.prev = &sentinel_;
    sentinel_.next = &sentinel_;
  }
  RegistrationList(const RegistrationList&) = delete;
  auto operator=(const RegistrationList&) -> RegistrationList& = delete;
  // Non-movable: the linked registrations point at this sentinel's address.
  RegistrationList(RegistrationList&&) = delete;
  auto operator=(RegistrationList&&) -> RegistrationList& = delete;
  // An activation can outlive the target it parked on, so detach what is still
  // linked rather than leave it pointing into freed storage.
  ~RegistrationList() {
    Clear();
  }

  [[nodiscard]] auto Empty() const noexcept -> bool {
    return sentinel_.next == &sentinel_;
  }

  void PushBack(Registration& reg) noexcept {
    reg.prev = sentinel_.prev;
    reg.next = &sentinel_;
    sentinel_.prev->next = &reg;
    sentinel_.prev = &reg;
  }

  // Claims the oldest registration, or null when there is none.
  auto PopFront() noexcept -> Registration* {
    Registration* reg = sentinel_.next;
    if (reg == &sentinel_) {
      return nullptr;
    }
    sentinel_.next = reg->next;
    reg->next->prev = &sentinel_;
    reg->prev = nullptr;
    reg->next = nullptr;
    return reg;
  }

  // Moves every registration onto the back of `other`, in order, leaving this
  // list empty.
  void SpliceBackOnto(RegistrationList& other) noexcept {
    if (Empty()) {
      return;
    }
    Registration* first = sentinel_.next;
    Registration* last = sentinel_.prev;
    first->prev = other.sentinel_.prev;
    last->next = &other.sentinel_;
    other.sentinel_.prev->next = first;
    other.sentinel_.prev = last;
    sentinel_.prev = &sentinel_;
    sentinel_.next = &sentinel_;
  }

  void Clear() noexcept {
    while (PopFront() != nullptr) {
    }
  }

  // Visits each registration. `fn` may unlink the one it is handed -- the walk
  // has already stepped past it -- but must not unlink any other.
  template <class Fn>
  void ForEach(Fn fn) {
    Registration* reg = sentinel_.next;
    while (reg != &sentinel_) {
      Registration* next = reg->next;
      fn(*reg);
      reg = next;
    }
  }

 private:
  Registration sentinel_;
};

}  // namespace lyra::runtime
