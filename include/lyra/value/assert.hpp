#pragma once

#include <string_view>

#include "lyra/base/source_location.hpp"

namespace lyra::value {

// A check the language requires before an access (LRM 11.9, 25.9): raises
// `message` as a simulation error unless `holds`.
void Assert(bool holds, std::string_view message, base::SourceLocation at);

}  // namespace lyra::value
