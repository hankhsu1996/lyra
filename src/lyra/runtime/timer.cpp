#include "lyra/runtime/timer.hpp"

#include <cstdint>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/region.hpp"
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
  std::visit(
      Overloaded{
          [&](LaterInThisTimeStep due) {
            effects.Schedule(effects.Now(), due.region, *this);
          },
          [&](AtTime due) {
            effects.Schedule(due.when, Region::kActive, *this);
          }},
      DueAfter(effects, ticks, precision_power_));
}

void Timer::Fire() {
  fire_();
}

}  // namespace lyra::runtime
