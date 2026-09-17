#pragma once

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// Waiting for this time slot's NBA region, which is where a nonblocking update
// is due (LRM 4.4.2.4). The placement names the slot rather than an instant, so
// waiting again is the same placement made afresh.
//
// Only the execution carrying an event-controlled update reaches the region
// this way, and only because the slot is unknown until the event has happened
// (LRM 9.4.5): an update whose slot is settled where the statement is reached
// hands the region a closure and waits for nothing.
class NbaRegionWait : public Wait {
 public:
  auto Begin(RuntimeEffects& services, CoroutineHandle leaf)
      -> WaitOutcome override {
    services.Schedule(services.Now(), Region::kNba, leaf);
    return WaitOutcome::kBlocked;
  }

  // A region boundary inside one update, not a construct LRM 12.4.2.1 names as
  // a point where a process flushes its violation reports.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }
};

inline auto ResumeInNbaRegion(RuntimeEffects& runtime) -> bool {
  return runtime.CurrentProcess().ParkOn<NbaRegionWait>(runtime);
}

}  // namespace lyra::runtime
