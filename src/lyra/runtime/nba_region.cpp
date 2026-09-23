#include "lyra/runtime/nba_region.hpp"

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

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

}  // namespace

auto ResumeInNbaRegion(RuntimeEffects& runtime) -> bool {
  return runtime.CurrentProcess().ParkOn<NbaRegionWait>(runtime);
}

}  // namespace lyra::runtime
