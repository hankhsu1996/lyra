#pragma once

#include <string>
#include <string_view>

namespace lyra::backend::cpp {

// Render a string as a C string literal: `"..."` with control characters,
// quotes, and backslashes escaped. Used for `PrintItem::Literal(...)` arguments
// and as the inner form of `RenderSvStringLiteral`.
auto RenderCStringLiteral(std::string_view s) -> std::string;

// Render a string as a `lyra::value::String{"..."}` initializer expression.
// Used for `mir::StringLiteral` operand emission so the resulting value
// matches the SystemVerilog `string` data type's runtime representation.
auto RenderSvStringLiteral(std::string_view s) -> std::string;

}  // namespace lyra::backend::cpp
