#include "lyra/runtime/delay.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

namespace {

// Multiplies by a power of ten, capping at the widest duration rather than
// wrapping. The cap is not defensive: LRM 9.4.1 gives a negative delay the
// value of its own bits read as an unsigned integer, which is a wait no
// simulation reaches, and wrapping would turn exactly that into a short one.
auto ScaleByPowerOfTen(SimDuration value, int exponent) noexcept
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
auto DeadlineAfter(SimTime now, SimDuration duration) noexcept -> SimTime {
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
auto ScaleToGlobalTicks(
    SimDuration ticks, std::int8_t from_power,
    std::int8_t global_power) noexcept -> SimDuration {
  return ScaleByPowerOfTen(ticks, from_power - global_power);
}

// A delay of some time: the process resumes at the deadline it was given, and
// started again after being stopped it still resumes there -- or at once, where
// the deadline has passed meanwhile (LRM 9.7).
class DelayAwaiter final : public Awaiter {
 public:
  explicit DelayAwaiter(SimTime deadline) : deadline_(deadline) {
  }

  auto Begin() -> Resumption override {
    return AtTime{.when = deadline_};
  }

  // A delay waits for time, not for a condition, so resuming from it is not a
  // flush point (LRM 12.4.2.1): a report raised before the delay stays pending.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  SimTime deadline_;
};

// A `#0` delay: the process gives way to everything already active in this
// time step and resumes later in it (LRM 4.4.2.3, 9.4.1). Started again after
// being stopped, its time has already come.
class ZeroDelayAwaiter final : public Awaiter {
 public:
  explicit ZeroDelayAwaiter(LaterInThisTimeStep due) : due_(due) {
  }

  auto Begin() -> Resumption override {
    return due_;
  }

  auto Again() -> Resumption override {
    return WithoutStopping{};
  }

  // Waiting for no time is still waiting for time (LRM 12.4.2.1).
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  LaterInThisTimeStep due_;
};

// A wait of `ticks` steps of `precision_power` from now. The two delay entries
// meet here.
auto DelayForTicks(
    RuntimeEffects& runtime, SimDuration ticks, std::int8_t precision_power)
    -> Wait {
  return std::visit(
      Overloaded{
          [](LaterInThisTimeStep due) {
            return MakeWait<ZeroDelayAwaiter>(due);
          },
          [](AtTime due) { return MakeWait<DelayAwaiter>(due.when); }},
      DueAfter(runtime, ticks, precision_power));
}

// Whether every one of `bits`' positions holds the scalar whose planes are
// `value_bit` and `unknown_bit`.
auto EveryBitIs(
    const value::ConstIntegralView& bits, bool value_bit, bool unknown_bit)
    -> bool {
  const std::size_t words = value::WordCountForBits(bits.width);
  for (std::size_t i = 0; i < words; ++i) {
    const std::uint64_t mask = value::ValidBitsMask(i, bits.width);
    const std::uint64_t value_word = value::WordAt(bits.value, i) & mask;
    const std::uint64_t unknown_word =
        bits.IsFourState() ? value::WordAt(bits.unknown, i) & mask : 0;
    if (value_word != (value_bit ? mask : 0) ||
        unknown_word != (unknown_bit ? mask : 0)) {
      return false;
    }
  }
  return true;
}

}  // namespace

auto DelayTicks(
    const value::ConstIntegralView& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> SimDuration {
  if (value::HasUnknown(duration)) {
    return 0;
  }
  return ScaleByPowerOfTen(
      static_cast<SimDuration>(value::ToInt64(duration)),
      static_cast<int>(unit_power - precision_power));
}

auto DelayTicksReal(
    const value::Real& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> SimDuration {
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

auto DelayDeadline(
    RuntimeEffects& runtime, SimDuration ticks, std::int8_t precision_power)
    -> SimTime {
  return DeadlineAfter(
      runtime.Now(),
      ScaleToGlobalTicks(
          ticks, precision_power, runtime.GlobalPrecisionPower()));
}

auto DueAfter(
    RuntimeEffects& runtime, SimDuration ticks, std::int8_t precision_power)
    -> DelayDue {
  if (ticks == 0) {
    return LaterInThisTimeStep{.region = RegionInSet::kInactive};
  }
  return AtTime{.when = DelayDeadline(runtime, ticks, precision_power)};
}

auto ScalarPropagationDelay(
    const value::ConstIntegralView& changed_to, SimDuration rise,
    SimDuration fall, SimDuration turn_off) -> SimDuration {
  switch (value::FourStateBitOf(
      (value::WordAt(changed_to.value, 0) & 1U) != 0U,
      changed_to.IsFourState() &&
          (value::WordAt(changed_to.unknown, 0) & 1U) != 0U)) {
    case value::FourStateBit::kOne:
      return rise;
    case value::FourStateBit::kZero:
      return fall;
    case value::FourStateBit::kHighImpedance:
      return turn_off;
    case value::FourStateBit::kUnknown:
      return std::min({rise, fall, turn_off});
  }
  throw InternalError("ScalarPropagationDelay: a scalar is one of four");
}

auto VectorPropagationDelay(
    const value::ConstIntegralView& changed_to, SimDuration rise,
    SimDuration fall, SimDuration turn_off) -> SimDuration {
  if (EveryBitIs(changed_to, false, false)) {
    return fall;
  }
  if (EveryBitIs(changed_to, false, true)) {
    return turn_off;
  }
  return rise;
}

auto Delay(
    RuntimeEffects& runtime, const value::ConstIntegralView& duration,
    std::int64_t unit_power, std::int64_t precision_power) -> Wait {
  const auto unit = static_cast<std::int8_t>(unit_power);
  const auto precision = static_cast<std::int8_t>(precision_power);
  return DelayForTicks(
      runtime, DelayTicks(duration, unit, precision), precision);
}

auto DelayReal(
    RuntimeEffects& runtime, const value::Real& duration,
    std::int64_t unit_power, std::int64_t precision_power) -> Wait {
  const auto unit = static_cast<std::int8_t>(unit_power);
  const auto precision = static_cast<std::int8_t>(precision_power);
  return DelayForTicks(
      runtime, DelayTicksReal(duration, unit, precision), precision);
}

}  // namespace lyra::runtime
