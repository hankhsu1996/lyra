#pragma once

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeServices;

// LRM 20.4.2: `$printtimescale`. Prints
// `Time scale of (<scope_name>) is <unit> / <precision>` using the LRM Table
// 20-2 unit spelling, terminated by a newline. The scope name and the two
// powers arrive as ordinary call-argument values, the same as any other
// runtime entry.
void LyraPrintTimescale(
    RuntimeServices& services, const value::String& scope_name,
    const value::PackedArray& unit_power,
    const value::PackedArray& precision_power);

// LRM 20.4.3 `$timeformat`. The two SV forms select between these by arity, the
// same overload resolution a backend applies to any call: the four-argument
// form stores the given `%t` display settings (the value arguments are narrowed
// here); the no-argument form restores the defaults (LRM Table 20-3) -- display
// unit is the design-global precision, which only the runtime resolves, plus
// precision 0, no suffix, field width 20.
void LyraTimeFormat(
    RuntimeServices& services, const value::PackedArray& units_power,
    const value::PackedArray& precision, const value::String& suffix,
    const value::PackedArray& min_width);
void LyraTimeFormat(RuntimeServices& services);

}  // namespace lyra::runtime
