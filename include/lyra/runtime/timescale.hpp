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

}  // namespace lyra::runtime
