#pragma once

#include <cmath>
#include <cstdint>
#include <limits>

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

// Multiplies by a power of ten, capping at the widest duration rather than
// wrapping. The cap is not defensive: LRM 9.4.1 gives a negative delay the
// value of its own bits read as an unsigned integer, which is a wait no
// simulation reaches, and wrapping would turn exactly that into a short one.
inline auto ScaleByPowerOfTen(SimDuration value, int exponent) noexcept
    -> SimDuration {
  SimDuration result = value;
  for (int i = 0; i < exponent; ++i) {
    if (result > std::numeric_limits<SimDuration>::max() / 10) {
      return std::numeric_limits<SimDuration>::max();
    }
    result *= 10;
  }
  return result;
}

// Adds a duration to a time, capping rather than wrapping: a deadline past the
// end of the time axis has to stay past it, or the longest wait the language
// can name comes back round as an imminent one.
inline auto DeadlineAfter(SimTime now, SimDuration duration) noexcept
    -> SimTime {
  if (now > std::numeric_limits<SimTime>::max() - duration) {
    return std::numeric_limits<SimTime>::max();
  }
  return now + duration;
}

// Scales `ticks`, expressed in `from_power` precision steps, up to the engine's
// `global_power` tick (LRM 3.14.3). `from_power >= global_power` because the
// global precision is the finest in the design, so the factor is a non-negative
// power of ten. A single-precision design has `from_power == global_power` and
// the factor is one.
inline auto ScaleToGlobalTicks(
    SimDuration ticks, std::int8_t from_power,
    std::int8_t global_power) noexcept -> SimDuration {
  return ScaleByPowerOfTen(ticks, from_power - global_power);
}

// The steps of `precision_power` a delay written as an integral expression
// waits. The value counts time units, and a whole number of units is already a
// whole number of precision steps, so LRM 3.14.1 rounding has nothing to
// remove here. LRM 9.4.1 gives two results meanings of their own instead of
// rejecting them: an unknown or high-impedance value is no delay, and a
// negative value is its own bits read as an unsigned integer the width of a
// time variable, which is the widest wait the language can name.
inline auto DelayTicks(
    const value::PackedArray& duration, std::int8_t unit_power,
    std::int8_t precision_power) -> SimDuration {
  if (duration.HasUnknown()) {
    return 0;
  }
  return ScaleByPowerOfTen(
      static_cast<SimDuration>(duration.ToInt64()),
      unit_power - precision_power);
}

// The same for a delay written as a real expression, which unlike an integral
// one can name a fraction of a time unit finer than the precision records. LRM
// 3.14.1 rounds it to the precision, halves away from zero, so an element whose
// unit is 1ns and whose precision is 100ps waits 2.8ns when asked for 2.75.
//
// LRM 9.4.1 answers a negative delay by reading its bits as an unsigned
// integer, and a real has no bits to read that way. What the rule achieves is a
// wait no simulation reaches, so any amount below zero -- and any value with no
// finite magnitude at all -- gets that wait directly.
inline auto DelayTicksReal(
    const value::Real& duration, std::int8_t unit_power,
    std::int8_t precision_power) -> SimDuration {
  constexpr auto kWidest = std::numeric_limits<SimDuration>::max();
  const auto units = static_cast<long double>(duration.Value());
  if (!std::isfinite(units)) {
    return kWidest;
  }
  const long double factor =
      std::pow(10.0L, static_cast<long double>(unit_power - precision_power));
  const long double steps = std::roundl(units * factor);
  if (steps < 0.0L) {
    return kWidest;
  }
  if (steps >= static_cast<long double>(kWidest)) {
    return kWidest;
  }
  return static_cast<SimDuration>(steps);
}

// Enqueues `token` to run again `ticks` steps of `precision_power` from now,
// and answers the absolute time that will be. A wait of no steps goes to the
// inactive region of the current slot, whose time is this instant, so a resume
// finds its deadline already transpired; anything else scales to the engine's
// global tick and goes to the slot that lands in. The engine does not know
// about delays as a category -- it only sees a process arriving in a queue at
// the right time.
inline auto ParkForDelay(
    RuntimeEffects& runtime, CoroutineHandle token, SimDuration ticks,
    std::int8_t precision_power) -> SimTime {
  if (ticks == 0) {
    runtime.ScheduleInactive(token);
    return runtime.Now();
  }
  const SimTime deadline = DeadlineAfter(
      runtime.Now(),
      ScaleToGlobalTicks(
          ticks, precision_power, runtime.GlobalPrecisionPower()));
  runtime.ScheduleAtTime(deadline, token);
  return deadline;
}

// Suspends the calling process for `ticks` steps of its scope's precision
// (`precision_power`).
class DelayAwaitable : public PendingWait {
 public:
  DelayAwaitable(
      RuntimeEffects& runtime, SimDuration ticks, std::int8_t precision_power)
      : runtime_(&runtime), ticks_(ticks), precision_power_(precision_power) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    CoroutineHandle token = &handle.promise();
    deadline_ = ParkForDelay(*runtime_, token, ticks_, precision_power_);
    BlockOn(token);
  }

  void await_resume() const {
    CheckAbortOnResume();
  }

  // A delay's deadline is absolute (LRM 9.7): on resume, if it has transpired
  // the process is runnable, otherwise it re-parks for the remaining time.
  auto Reestablish(RuntimeEffects& runtime, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    if (runtime.Now() >= deadline_) {
      return PendingWaitOutcome::kRunnable;
    }
    runtime.ScheduleAtTime(deadline_, activation);
    return PendingWaitOutcome::kReblocked;
  }

  // A delay waits for time, not for a condition, so resuming from it is not a
  // flush point (LRM 12.4.2.1): a report raised before the delay stays pending.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  RuntimeEffects* runtime_;
  SimDuration ticks_;
  std::int8_t precision_power_;
  SimTime deadline_ = 0;
};

inline auto Delay(
    RuntimeEffects& runtime, const value::PackedArray& duration,
    const value::PackedArray& unit_power,
    const value::PackedArray& precision_power) -> DelayAwaitable {
  const auto unit = static_cast<std::int8_t>(unit_power.ToInt64());
  const auto precision = static_cast<std::int8_t>(precision_power.ToInt64());
  return DelayAwaitable{
      runtime, DelayTicks(duration, unit, precision), precision};
}

inline auto DelayReal(
    RuntimeEffects& runtime, const value::Real& duration,
    const value::PackedArray& unit_power,
    const value::PackedArray& precision_power) -> DelayAwaitable {
  const auto unit = static_cast<std::int8_t>(unit_power.ToInt64());
  const auto precision = static_cast<std::int8_t>(precision_power.ToInt64());
  return DelayAwaitable{
      runtime, DelayTicksReal(duration, unit, precision), precision};
}

}  // namespace lyra::runtime
