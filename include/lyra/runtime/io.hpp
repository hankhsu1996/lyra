#pragma once

#include <functional>
#include <span>

#include "lyra/value/format.hpp"

namespace lyra::runtime {

class RuntimeServices;

// Single high-level runtime print API. The compiler emits one call per
// $display/$write/...; the runtime walks the items, formats values via
// value::Format, and finalizes the record (newline for kDisplay/kFDisplay).
void LyraPrint(
    RuntimeServices& services, value::PrintKind kind,
    std::span<const value::PrintItem> items);

// LRM 21.2.2 $strobe-family runtime entry. Defers `print_action` to the
// postponed region of the current time slot so the print observes
// NBA-committed values. stdout-sink variants ($strobe[bho]) -- nothing to
// cancel (stdout has no $fclose), so this is a thin SubmitPostponed
// wrapper; the entry exists for symmetry with LyraSubmitFStrobe.
void LyraSubmitStrobe(
    RuntimeServices& services, std::function<void()> print_action);

}  // namespace lyra::runtime
