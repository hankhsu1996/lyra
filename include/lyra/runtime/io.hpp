#pragma once

#include <span>

#include "lyra/runtime/format.hpp"

namespace lyra::runtime {

class RuntimeServices;

// Single high-level runtime print API. The compiler emits one call per
// $display/$write/...; the runtime walks the items, formats values via
// FormatValue, and finalizes the record (newline for kDisplay/kFDisplay).
void LyraPrint(
    RuntimeServices& services, PrintKind kind,
    std::span<const PrintItem> items);

}  // namespace lyra::runtime
