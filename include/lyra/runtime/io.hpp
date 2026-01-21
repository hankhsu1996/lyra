#pragma once

#include <cstdint>

extern "C" {

// Print a literal string (FormatKind::kLiteral only)
void LyraPrintLiteral(const char* str);

// Print a formatted value (all FormatKind except kLiteral)
// - format: FormatKind cast to int32_t
// - data: pointer to value bits
// - width: bit width of the value
// - is_signed: true for signed interpretation
// - x_mask: pointer to X bits (null for 2-state)
// - z_mask: pointer to Z bits (null for 2-state)
void LyraPrintValue(
    int32_t format, const void* data, int32_t width, bool is_signed,
    const void* x_mask, const void* z_mask);

// Finalize output: newline for kDisplay (0), nothing for kWrite (1)
void LyraPrintEnd(int32_t kind);

// Register a variable for snapshot. Called at program init.
// kind: 0 = integral, 1 = real
void LyraRegisterVar(
    const char* name, void* addr, int32_t kind, int32_t width, bool is_signed);

// Output all registered variables. Called before exit.
void LyraSnapshotVars();
}
