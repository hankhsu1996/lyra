#pragma once

#include <cstdint>

#include "lyra/runtime/string.hpp"

// Element kind for $readmem/$writemem - determines storage layout
enum class MemElementKind : int32_t {
  kTwoState = 0,   // Packed 2-state integral (single value)
  kFourState = 1,  // Packed 4-state integral (value + x_mask struct)
};

extern "C" {

// Print a literal string (FormatKind::kLiteral only)
void LyraPrintLiteral(const char* str);

// Print a formatted value (all FormatKind except kLiteral)
// - engine: pointer to Engine (required for kTime, can be nullptr for others)
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
// - unknown_data: pointer to unknown plane (null for 2-state). When set, bit i
//     is X if unknown[i]=1 && data[i]=0, Z if unknown[i]=1 && data[i]=1.
// - z_mask: unused (reserved for future)
// - module_timeunit_power: timeunit of the value (for kTime: e.g., -9 for ns)
void LyraPrintValue(
    void* engine, int32_t format, int32_t value_kind, const void* data,
    int32_t width, bool is_signed, int32_t output_width, int32_t precision,
    bool zero_pad, bool left_align, const void* unknown_data,
    const void* z_mask, int8_t module_timeunit_power);

// Finalize output: newline for kDisplay (0), nothing for kWrite (1)
void LyraPrintEnd(int32_t kind);

// Register a variable for snapshot. Called at program init.
// kind: 0 = integral, 1 = real
// is_four_state: true for 4-state types (logic, reg), slot layout is
//                {value_plane, unknown_plane}
void LyraRegisterVar(
    const char* name, void* addr, int32_t kind, int32_t width, bool is_signed,
    bool is_four_state);

// Output all registered variables. Called before exit.
void LyraSnapshotVars();

// $fopen with mode (FD mode) - returns int32, 0 on failure
// engine: opaque pointer to lyra::runtime::Engine
auto LyraFopenFd(void* engine, LyraStringHandle filename, LyraStringHandle mode)
    -> int32_t;

// $fopen without mode (MCD mode) - returns int32, 0 on failure
// engine: opaque pointer to lyra::runtime::Engine
auto LyraFopenMcd(void* engine, LyraStringHandle filename) -> int32_t;

// $fclose - no-op for invalid descriptor
// engine: opaque pointer to lyra::runtime::Engine
void LyraFclose(void* engine, int32_t descriptor);

// Write formatted message to file descriptor(s).
// - engine: pointer to Engine (for FileManager access)
// - descriptor: MCD or FD descriptor (0 = no-op, silently ignored)
// - message: string handle (read-only, NOT retained by this function)
// - add_newline: true for $fdisplay, false for $fwrite
// Silently skips invalid/closed streams (matches MIR interpreter behavior).
void LyraFWrite(
    void* engine, uint32_t descriptor, LyraStringHandle message,
    bool add_newline);

// $readmemh/$readmemb: read memory file into array
// - filename: string handle for filename
// - target: pointer to array storage
// - element_width: bit width of each element (for parsing, must be > 0)
// - stride_bytes: total storage bytes per element (from LLVM DataLayout)
// - value_size_bytes: bytes for value plane (= stride_bytes for 2-state,
//                     stride_bytes/2 for 4-state struct {value, x_mask})
// - element_count: number of elements in array (must be > 0)
// - min_addr: array lower bound (= Lower() for both ascending/descending)
// - current_addr: starting address (lowering applies defaults)
// - final_addr: terminal address (inclusive)
// - step: +1 for ascending, -1 for descending progression
// - is_hex: true = hex format, false = binary format
// - element_kind: MemElementKind cast to int32_t
//
// Storage ABI: 2-state elements are packed integrals stored in power-of-2
// rounded bytes. 4-state elements are struct {value, x_mask} where each plane
// uses the same power-of-2 storage type. Lowering computes all parameters;
// runtime has zero semantic decisions.
void LyraReadmem(
    LyraStringHandle filename, void* target, int32_t element_width,
    int32_t stride_bytes, int32_t value_size_bytes, int32_t element_count,
    int64_t min_addr, int64_t current_addr, int64_t final_addr, int64_t step,
    bool is_hex, int32_t element_kind);

// $writememh/$writememb: write array to memory file
// Parameters match LyraReadmem, but source is read-only.
void LyraWritemem(
    LyraStringHandle filename, const void* source, int32_t element_width,
    int32_t stride_bytes, int32_t value_size_bytes, int32_t element_count,
    int64_t min_addr, int64_t current_addr, int64_t final_addr, int64_t step,
    bool is_hex, int32_t element_kind);

// Print hierarchical module path (%m format specifier)
// - engine: pointer to Engine (for instance path lookup)
// - instance_id: index into instance_paths (from process's owner_instance_id)
void LyraPrintModulePath(void* engine, uint32_t instance_id);
}
