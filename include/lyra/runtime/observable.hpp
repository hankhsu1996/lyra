#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

// A place where something happens that activations wait for: a variable cell or
// a net taking a new value (LRM 4.3 calls that an update event), a named event
// being triggered (LRM 15.5.1). What waits here is the whole of what such an
// occurrence is reported to -- nothing else reads one -- so an occurrence with
// nothing waiting has nothing to report, and may skip the work of describing
// itself.
//
// Deriving from this has to leave the derived cell's own address equal to the
// address of what waits on it: generated code hands a cell's address across a C
// boundary, where the pointer carries no type to adjust by.
class Observable {
 public:
  Observable() = default;
  Observable(const Observable&) = delete;
  auto operator=(const Observable&) -> Observable& = delete;
  Observable(Observable&&) = delete;
  auto operator=(Observable&&) -> Observable& = delete;
  ~Observable() = default;

  [[nodiscard]] auto HasWaiter() const noexcept -> bool {
    return !waiters_.Empty();
  }

  void Subscribe(
      CoroutineHandle handle, Observation observation,
      std::uint64_t lsb_bit_offset, std::uint64_t bit_width) {
    Registration& reg = handle->Park(waiters_);
    reg.lsb_bit_offset = lsb_bit_offset;
    reg.bit_width = bit_width;
    reg.observation = std::move(observation);
  }

  // Claims and returns the activations this occurrence is an event for; the
  // rest stay parked. A wait whose bits it left alone is passed over without
  // being asked, and every other one answers for itself -- an event control by
  // what its expression is worth now, an implicit sensitivity or a named-event
  // wait by having been reached at all (LRM 9.2.2.2.1, 9.4.2, 15.5.1).
  [[nodiscard]] auto TakeFiringWaiters(const ProjectionUnchanged& unchanged)
      -> std::vector<CoroutineHandle> {
    std::vector<CoroutineHandle> woken;
    waiters_.ForEach([&](Registration& reg) {
      if (reg.bit_width != 0 && unchanged(reg.lsb_bit_offset, reg.bit_width)) {
        return;
      }
      if (!reg.FiresNow()) {
        return;
      }
      reg.Unlink();
      woken.push_back(reg.activation);
    });
    return woken;
  }

 private:
  RegistrationList waiters_;
};

// The answer for a change whose parts are not bit ranges: nothing about a
// leaf's bits can be shown untouched, so every wait on it is asked.
inline auto MakeWholeValueProjectionTest() -> ProjectionUnchanged {
  return [](std::uint64_t, std::uint64_t) -> bool { return false; };
}

}  // namespace lyra::runtime
