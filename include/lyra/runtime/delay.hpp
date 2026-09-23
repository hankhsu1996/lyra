#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

// The steps of `precision_power` a delay written as an integral expression
// waits. The value counts time units, and a whole number of units is already a
// whole number of precision steps, so LRM 3.14.1 rounding has nothing to
// remove here. LRM 9.4.1 gives two results meanings of their own instead of
// rejecting them: an unknown or high-impedance value is no delay, and a
// negative value is its own bits read as an unsigned integer the width of a
// time variable, which is the widest wait the language can name.
auto DelayTicks(
    const value::PackedArray& duration, std::int8_t unit_power,
    std::int8_t precision_power) -> SimDuration;

// The same for a delay written as a real expression, which unlike an integral
// one can name a fraction of a time unit finer than the precision records. LRM
// 3.14.1 rounds it to the precision, halves away from zero, so an element whose
// unit is 1ns and whose precision is 100ps waits 2.8ns when asked for 2.75.
//
// LRM 9.4.1 answers a negative delay by reading its bits as an unsigned
// integer, and a real has no bits to read that way. What the rule achieves is a
// wait no simulation reaches, so any amount below zero -- and any value with no
// finite magnitude at all -- gets that wait directly.
auto DelayTicksReal(
    const value::Real& duration, std::int8_t unit_power,
    std::int8_t precision_power) -> SimDuration;

// The absolute simulation time a delay of `ticks` steps of `precision_power`
// reaches. The scope's steps scale up to the engine's global tick before they
// are added, because a design may declare several precisions and the engine
// counts in the finest of them (LRM 3.14.3).
auto DelayDeadline(
    RuntimeEffects& runtime, SimDuration ticks, std::int8_t precision_power)
    -> SimTime;

// Waiting for a moment in simulation time. What is kept is the moment, not the
// amount of time left to it, which is what LRM 9.7 asks for when a process
// stopped part-way through a delay is started again: it goes on waiting for
// that same moment, and where the moment has passed it continues at once.
//
// LRM 4.4.2.3: a delay of no steps is an explicit `#0`, which waits in the
// inactive region of the current time slot so that active work already pending
// finishes first. The engine does not know about delays as a category -- it
// only sees an activation arriving in a region at the right time.
//
// The two entries differ only in how the amount the design wrote becomes a
// count of steps; both answer whether the caller must give up control.
auto Delay(
    RuntimeEffects& runtime, const value::PackedArray& duration,
    const value::PackedArray& unit_power,
    const value::PackedArray& precision_power) -> bool;

auto DelayReal(
    RuntimeEffects& runtime, const value::Real& duration,
    const value::PackedArray& unit_power,
    const value::PackedArray& precision_power) -> bool;

}  // namespace lyra::runtime
