#pragma once

#include <string_view>

#include "lyra/backend/cpp/target_text.hpp"

namespace lyra::backend::cpp {

// A C string literal, `"..."`, with control characters, quotes and backslashes
// escaped. A construction call around it turns it into a string value.
void WriteCStringLiteral(std::string_view s, TargetText& out);

}  // namespace lyra::backend::cpp
