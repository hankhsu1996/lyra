#include "lyra/lowering/ast_to_hir/time_resolution.hpp"

#include <cstdint>
#include <optional>

#include <slang/numeric/Time.h>

#include "lyra/base/time.hpp"

namespace lyra::lowering::ast_to_hir {

auto TimeScaleValueToPower(slang::TimeScaleValue v) -> std::int8_t {
  const int unit_power = -3 * static_cast<int>(v.unit);
  int mag_power = 0;
  switch (v.magnitude) {
    case slang::TimeScaleMagnitude::One:
      mag_power = 0;
      break;
    case slang::TimeScaleMagnitude::Ten:
      mag_power = 1;
      break;
    case slang::TimeScaleMagnitude::Hundred:
      mag_power = 2;
      break;
  }
  return static_cast<std::int8_t>(unit_power + mag_power);
}

auto ResolveTimeResolution(std::optional<slang::TimeScale> ts)
    -> TimeResolution {
  if (!ts) {
    return TimeResolution{};
  }
  return TimeResolution{
      .unit_power = TimeScaleValueToPower(ts->base),
      .precision_power = TimeScaleValueToPower(ts->precision)};
}

}  // namespace lyra::lowering::ast_to_hir
