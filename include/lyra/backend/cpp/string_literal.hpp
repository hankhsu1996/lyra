#pragma once

#include <string>
#include <string_view>

namespace lyra::backend::cpp {

// Render a string as a C string literal: `"..."` with control characters,
// quotes, and backslashes escaped. This is how a `mir::StringLiteral`
// renders -- a software string literal -- which a `ConstructorCallee` then
// turns into a `value::String`.
auto RenderCStringLiteral(std::string_view s) -> std::string;

}  // namespace lyra::backend::cpp
