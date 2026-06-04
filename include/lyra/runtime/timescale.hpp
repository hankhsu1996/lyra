#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::runtime {

class RuntimeServices;

// LRM 20.4.2: `$printtimescale`. Prints
// `Time scale of (<scope_name>) is <unit> / <precision>` using the LRM Table
// 20-2 unit spelling, terminated by a newline.
void LyraPrintTimescale(
    RuntimeServices& services, std::string_view scope_name,
    std::int8_t unit_power, std::int8_t precision_power);

}  // namespace lyra::runtime
