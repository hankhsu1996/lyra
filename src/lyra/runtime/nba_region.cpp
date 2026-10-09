#include "lyra/runtime/nba_region.hpp"

#include "lyra/runtime/region.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

class NbaRegionAwaiter final : public Awaiter {
 public:
  auto Begin() -> Resumption override {
    return LaterInThisTimeStep{.region = Region::kNba};
  }

  // A region boundary inside one update, not a construct LRM 12.4.2.1 names as
  // a point where a process flushes its violation reports.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }
};

}  // namespace

auto ResumeInNbaRegion() -> Wait {
  return MakeWait<NbaRegionAwaiter>();
}

}  // namespace lyra::runtime
