#pragma once

#include <span>

#include "lyra/value/format.hpp"

namespace lyra::runtime {

class RuntimeServices;

// Single high-level runtime print API. The compiler emits one call per
// $display/$write/...; the runtime walks the items, formats values via
// FormatValue, and finalizes the record (newline for kDisplay/kFDisplay).
void LyraPrint(
    RuntimeServices& services, value::PrintKind kind,
    std::span<const value::PrintItem> items);

}  // namespace lyra::runtime
