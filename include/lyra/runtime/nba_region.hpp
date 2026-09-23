#pragma once

#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

// Waiting for this time slot's NBA region, which is where a nonblocking update
// is due (LRM 4.4.2.4). The placement names the slot rather than an instant, so
// waiting again is the same placement made afresh.
//
// Only the execution carrying an event-controlled update reaches the region
// this way, and only because the slot is unknown until the event has happened
// (LRM 9.4.5): an update whose slot is settled where the statement is reached
// hands the region a closure and waits for nothing.
auto ResumeInNbaRegion(RuntimeEffects& runtime) -> bool;

}  // namespace lyra::runtime
