#pragma once

#include <string>
#include <string_view>

namespace lyra::backend::cpp {

// Render a string as a C string literal: `"..."` with control characters,
// quotes, and backslashes escaped. Used for `LyraPrintLiteral(...)` arguments
// and as the inner form of `RenderStdStringLiteral`.
auto RenderCStringLiteral(std::string_view s) -> std::string;

// Render a string as a `std::string{"..."}` initializer expression. Used for
// `mir::StringLiteral` operand emission.
auto RenderStdStringLiteral(std::string_view s) -> std::string;

}  // namespace lyra::backend::cpp
