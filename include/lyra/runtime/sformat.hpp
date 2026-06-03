#pragma once

#include <span>

#include "lyra/value/format.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// LRM 21.3.3 string-format family runtime entry. Reuses the same
// `value::FormatValue` engine as `LyraPrint`, but appends to a local
// std::string buffer that becomes the returned `value::String` rvalue.
// No newline is appended -- the formatted text is the entire output.
[[nodiscard]] auto LyraSFormat(std::span<const value::PrintItem> items)
    -> value::String;

}  // namespace lyra::runtime
