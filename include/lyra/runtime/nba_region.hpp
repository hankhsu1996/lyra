#pragma once

#include <cstdint>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

// Waiting for this time slot's NBA region, which is where a nonblocking update
// is due (LRM 4.4.2.4). The placement names the slot rather than an instant, so
// waiting again is the same placement made afresh.
//
// Only the execution carrying an event-controlled update reaches the region
// this way, and only because the slot is unknown until the event has happened
// (LRM 9.4.5): an update whose slot is settled where the statement is reached
// hands the region a closure and waits for nothing.
auto ResumeInNbaRegion() -> Wait;

// Waiting for `region` itself, a position of `support::Region`, of the slot
// `duration` from now names, whichever region set the waiting thread runs in:
// where a construct the standard names the region of goes on, as a synchronous
// drive lands in Re-NBA once its clocking event has matured it and its output
// skew has passed, a drive made by design code and one made by a program alike
// (LRM 14.4, 14.16). `duration` is read as a delay control's amount (LRM
// 9.4.1), and an amount of none names this slot.
auto ResumeInRegionAfter(
    RuntimeEffects& runtime, std::int64_t region,
    const value::ConstIntegralView& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> Wait;

template <value::IntegralValue T>
auto ResumeInRegionAfter(
    RuntimeEffects& runtime, std::int64_t region, const T& duration,
    std::int64_t unit_power, std::int64_t precision_power) -> Wait {
  return ResumeInRegionAfter(
      runtime, region, duration.AsWords().View(), unit_power, precision_power);
}

auto ResumeInRegionAfterReal(
    RuntimeEffects& runtime, std::int64_t region, const value::Real& duration,
    std::int64_t unit_power, std::int64_t precision_power) -> Wait;

}  // namespace lyra::runtime
