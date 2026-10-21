#pragma once

#include <string_view>

namespace lyra::frontend {

// The name of the text holding Lyra's definitions of the classes the front end
// declares in the built-in package `std` (LRM 15.3, 15.4). Every compilation
// reads it as a compilation unit of its own, compiled in the name of `std`.
inline constexpr std::string_view kStdDefinitionsBufferName = "<std>";

}  // namespace lyra::frontend
