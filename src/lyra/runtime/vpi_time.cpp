#include "lyra/runtime/vpi_time.hpp"

#include <cstdint>

#include "lyra/abi/records.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/sim_time.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

auto TimeResolutionOf(
    RuntimeEffects& run, const abi::DefinitionRecord* definition)
    -> TimeResolution {
  if (definition == nullptr) {
    const std::int8_t global = run.GlobalPrecisionPower();
    return {.unit_power = global, .precision_power = global};
  }
  return {
      .unit_power = definition->time_unit_power,
      .precision_power = definition->time_precision_power};
}

auto FillTime(RuntimeEffects& run, std::int8_t unit_power, s_vpi_time& time)
    -> bool {
  switch (time.type) {
    case vpiSimTime: {
      const SimTime now = NowInUnit(run, unit_power);
      time.high = static_cast<PLI_UINT32>(now >> 32U);
      time.low = static_cast<PLI_UINT32>(now & 0xFFFFFFFFU);
      return true;
    }
    case vpiScaledRealTime:
      time.real = RealTimeInUnit(run, unit_power).Value();
      return true;
    case vpiSuppressTime:
      return true;
    default:
      return false;
  }
}

}  // namespace lyra::runtime
