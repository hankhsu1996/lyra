#pragma once

#include <string_view>

#include "lyra/backend/cpp/target_text.hpp"

namespace lyra::backend::cpp {

// Write a string as a C string literal: `"..."` with control characters,
// quotes, and backslashes escaped. This is how a software string literal
// reaches the target, which a construction then turns into a string value.
void WriteCStringLiteral(std::string_view s, TargetText& out);

}  // namespace lyra::backend::cpp
