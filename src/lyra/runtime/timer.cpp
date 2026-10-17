#include "lyra/runtime/timer.hpp"

#include <cstdint>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

Timer::Timer() {
  place_.timer = this;
}

Timer::~Timer() = default;

void Timer::Bind(OwnedCall fire, std::int64_t precision_power) {
  fire_ = std::move(fire);
  precision_power_ = static_cast<std::int8_t>(precision_power);
}

void Timer::Arm(RuntimeEffects& effects, SimDuration ticks) {
  place_.Unlink();
  // Armed by the drive it propagates, so it fires in that drive's region set.
  const RegionSet regions = AsRuntime(effects).RunningContext().regions;
  std::visit(
      Overloaded{
          [&](LaterInThisTimeStep due) {
            effects.Schedule(effects.Now(), regions[due.region], *this);
          },
          [&](AtTime due) {
            effects.Schedule(due.when, regions.active, *this);
          }},
      DueAfter(effects, ticks, precision_power_));
}

void Timer::Fire() {
  fire_();
}

}  // namespace lyra::runtime
