#pragma once

#include <cstdint>

extern "C" {

// Print a literal string (FormatKind::kLiteral only)
void LyraPrintLiteral(const char* str);

// Print a formatted value (all FormatKind except kLiteral)
// - format: FormatKind cast to int32_t (how to render)
// - value_kind: RuntimeValueKind cast to int32_t (how to interpret bytes)
// - data: pointer to value bits
// - width: bit width for integrals (must be <= 64 for kIntegral)
// - is_signed: true for signed interpretation
// - output_width: encodes FormatModifiers.width (optional<int>) as int32_t:
//     -1 = nullopt (no width specified) → auto-size hex/binary/octal to
//     bit-width
//      0 = explicit %0h/%0b style → minimal output, no leading zeros
//     >0 = explicit width → pad/truncate to exactly N digits
// - precision: precision for real formats (-1 means default)
// - zero_pad: pad with zeros instead of spaces (from %0Nd syntax)
// - left_align: left-align within field width (from %-Nd syntax)
// - x_mask: pointer to X bits (null for 2-state)
// - z_mask: pointer to Z bits (null for 2-state)
void LyraPrintValue(
    int32_t format, int32_t value_kind, const void* data, int32_t width,
    bool is_signed, int32_t output_width, int32_t precision, bool zero_pad,
    bool left_align, const void* x_mask, const void* z_mask);

// Finalize output: newline for kDisplay (0), nothing for kWrite (1)
void LyraPrintEnd(int32_t kind);

// Register a variable for snapshot. Called at program init.
// kind: 0 = integral, 1 = real
void LyraRegisterVar(
    const char* name, void* addr, int32_t kind, int32_t width, bool is_signed);

// Output all registered variables. Called before exit.
void LyraSnapshotVars();
}
