#include "lyra/runtime/nba_region.hpp"

#include <cstdint>

#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

// A region boundary inside one update, not a construct LRM 12.4.2.1 names as a
// point where a process flushes its violation reports.
class UpdateRegionAwaiter final : public Awaiter {
 public:
  explicit UpdateRegionAwaiter(Resumption due) : due_(due) {
  }

  auto Begin() -> Resumption override {
    return due_;
  }

  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  Resumption due_;
};

}  // namespace

auto ResumeInNbaRegion() -> Wait {
  return MakeWait<UpdateRegionAwaiter>(
      LaterInThisTimeStep{.region = RegionInSet::kNba});
}

auto ResumeInRegionAfter(
    RuntimeEffects& runtime, std::int64_t region,
    const value::ConstIntegralView& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> Wait {
  return MakeWait<UpdateRegionAwaiter>(InRegion{
      .region = RegionOperand(region),
      .when =
          DeadlineAfterAmount(runtime, duration, unit_power, precision_power)});
}

auto ResumeInRegionAfterReal(
    RuntimeEffects& runtime, std::int64_t region, const value::Real& duration,
    std::int64_t unit_power, std::int64_t precision_power) -> Wait {
  return MakeWait<UpdateRegionAwaiter>(InRegion{
      .region = RegionOperand(region),
      .when = DeadlineAfterRealAmount(
          runtime, duration, unit_power, precision_power)});
}

}  // namespace lyra::runtime
