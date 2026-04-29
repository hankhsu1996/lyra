#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"

#include <cmath>
#include <cstdint>
#include <limits>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/primary.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto HirTimeScaleToPower(hir::TimeScale s) -> std::int8_t {
  switch (s) {
    case hir::TimeScale::kFs:
      return -15;
    case hir::TimeScale::kPs:
      return -12;
    case hir::TimeScale::kNs:
      return -9;
    case hir::TimeScale::kUs:
      return -6;
    case hir::TimeScale::kMs:
      return -3;
    case hir::TimeScale::kS:
      return 0;
  }
  throw InternalError("HirTimeScaleToPower: unknown hir::TimeScale");
}

auto IntegerPow10(int exponent) -> std::optional<std::uint64_t> {
  if (exponent < 0) {
    return std::nullopt;
  }
  std::uint64_t out = 1;
  for (int i = 0; i < exponent; ++i) {
    if (out > std::numeric_limits<std::uint64_t>::max() / 10) {
      return std::nullopt;
    }
    out *= 10;
  }
  return out;
}

}  // namespace

DelayTimeResolver::DelayTimeResolver(TimeResolution resolution)
    : resolution_(resolution) {
}

auto DelayTimeResolver::ResolveIntegerDelay(
    std::int64_t value, diag::SourceSpan span) const
    -> diag::Result<SimDuration> {
  if (value < 0) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value must be non-negative");
  }
  const int ratio_exp = resolution_.unit_power - resolution_.precision_power;
  if (ratio_exp < 0) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value cannot be represented under the active time precision");
  }
  const auto ratio_or = IntegerPow10(ratio_exp);
  if (!ratio_or) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value overflows internal tick representation");
  }
  const std::uint64_t ratio = *ratio_or;
  const auto unsigned_value = static_cast<std::uint64_t>(value);
  if (ratio != 0 &&
      unsigned_value > std::numeric_limits<std::uint64_t>::max() / ratio) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value overflows internal tick representation");
  }
  return static_cast<SimDuration>(unsigned_value * ratio);
}

auto DelayTimeResolver::ResolveTimeLiteral(
    double value, hir::TimeScale literal_unit, diag::SourceSpan span) const
    -> diag::Result<SimDuration> {
  if (!std::isfinite(value)) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value is not a finite real number");
  }
  if (value < 0.0) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value must be non-negative");
  }
  const int ratio_exp = static_cast<int>(HirTimeScaleToPower(literal_unit)) -
                        static_cast<int>(resolution_.precision_power);
  const long double ratio =
      std::pow(10.0L, static_cast<long double>(ratio_exp));
  const long double ticks_f = static_cast<long double>(value) * ratio;
  if (!std::isfinite(ticks_f)) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value overflows internal tick representation");
  }
  // Cap at int64_t::max() because std::llroundl returns long long, not the
  // semantic max of SimDuration (uint64_t).
  if (ticks_f < 0.0L ||
      ticks_f >
          static_cast<long double>(std::numeric_limits<std::int64_t>::max())) {
    return diag::Error(
        span, diag::DiagCode::kDelayValueOutOfRange,
        "delay value overflows internal tick representation");
  }
  const std::int64_t ticks = std::llroundl(ticks_f);
  return static_cast<SimDuration>(ticks);
}

}  // namespace lyra::lowering::hir_to_mir
