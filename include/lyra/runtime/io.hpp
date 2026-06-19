#pragma once

#include <functional>
#include <span>

#include "lyra/value/format.hpp"

namespace lyra::runtime {

class RuntimeServices;

// Walks the item sequence, formats each value via value::Format, and writes the
// record to stdout, appending a trailing newline for the newline kind
// (kDisplay, LRM 21.2.1.1).
void LyraPrint(
    RuntimeServices& services, value::PrintKind kind,
    std::span<const value::PrintItem> items);

// The print-to-sink entries for $display / $write / $fdisplay / $fwrite. The
// Display / FDisplay pair append a trailing newline (LRM 21.2.1.1); Write /
// FWrite do not. The F-prefixed pair writes to the descriptor's sink (LRM
// 21.3.2); the others write to stdout. Each formats the item sequence and
// emits it.
void LyraDisplay(
    RuntimeServices& services, std::span<const value::PrintItem> items);
void LyraWrite(
    RuntimeServices& services, std::span<const value::PrintItem> items);
void LyraFDisplay(
    RuntimeServices& services, const value::PackedArray& descriptor,
    std::span<const value::PrintItem> items);
void LyraFWrite(
    RuntimeServices& services, const value::PackedArray& descriptor,
    std::span<const value::PrintItem> items);

// LRM 21.2.2 $strobe-family runtime entry. Defers `print_action` to the
// postponed region of the current time slot so the print observes
// NBA-committed values. stdout-sink variants ($strobe[bho]) -- nothing to
// cancel (stdout has no $fclose), so this is a thin SubmitPostponed
// wrapper; the entry exists for symmetry with LyraSubmitFStrobe.
void LyraSubmitStrobe(
    RuntimeServices& services, std::function<void()> print_action);

}  // namespace lyra::runtime
