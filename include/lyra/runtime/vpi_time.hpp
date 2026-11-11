#pragma once

// How a scope's time is read through VPI and DPI-C, whose time value is the
// same structure (LRM 38.13, H.13).

#include <cstdint>

#include "lyra/abi/records.hpp"
#include "lyra/base/time.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

class RuntimeEffects;

// The time unit and precision a query of a scope of `definition` reads, as
// powers of ten, or the simulation time unit for both where it asks of no
// scope (LRM 38.6, 38.13, H.13).
[[nodiscard]] auto TimeResolutionOf(
    RuntimeEffects& run, const abi::DefinitionRecord* definition)
    -> TimeResolution;

// Fills `time` with the current time in the unit `unit_power` names, in the
// form its type asks (LRM 38.13): an integer rounded as `$time` rounds for
// `vpiSimTime`, a real for `vpiScaledRealTime`, and nothing for
// `vpiSuppressTime`; false where the type names no form.
[[nodiscard]] auto FillTime(
    RuntimeEffects& run, std::int8_t unit_power, s_vpi_time& time) -> bool;

}  // namespace lyra::runtime
