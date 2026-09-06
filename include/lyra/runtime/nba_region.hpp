#pragma once

#include <coroutine>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

// Suspends the calling execution until this time slot's NBA region, which is
// where a nonblocking assignment's update is due (LRM 4.4.2.4).
//
// Only the execution carrying an event-controlled update reaches the region
// this way, and only because the slot is unknown until the event has happened
// (LRM 9.4.5): an update whose slot is settled where the statement is reached
// hands the region a closure and suspends nothing.
class NbaRegionAwaitable : public PendingWait {
 public:
  explicit NbaRegionAwaitable(RuntimeEffects& runtime) : runtime_(&runtime) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    CoroutineHandle token = &handle.promise();
    runtime_->Schedule(runtime_->Now(), Region::kNba, token);
    BlockOn(token);
  }

  void await_resume() const {
    CheckAbortOnResume();
  }

  // The placement names this slot rather than an instant, so re-establishing is
  // the same placement made again.
  auto Reestablish(RuntimeEffects& runtime, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    runtime.Schedule(runtime.Now(), Region::kNba, activation);
    return PendingWaitOutcome::kReblocked;
  }

  // A region boundary inside one update, not a construct LRM 12.4.2.1 names as
  // a point where a process flushes its violation reports.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  RuntimeEffects* runtime_;
};

inline auto ResumeInNbaRegion(RuntimeEffects& runtime) -> NbaRegionAwaitable {
  return NbaRegionAwaitable{runtime};
}

}  // namespace lyra::runtime
