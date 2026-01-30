#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/runtime/format_spec_abi.hpp"

using LyraStringHandle = void*;

extern "C" {

// Creates a string from a literal. Returns owned handle (+1 refcount).
auto LyraStringFromLiteral(const char* data, int64_t len) -> LyraStringHandle;

// Returns <0 if a < b, 0 if equal, >0 if a > b (memcmp semantics)
auto LyraStringCmp(LyraStringHandle a, LyraStringHandle b) -> int32_t;

// Increment refcount. No-op for null. Returns the same handle.
auto LyraStringRetain(LyraStringHandle handle) -> LyraStringHandle;

// Concatenates N strings. Returns owned handle (+1 refcount). count==0 -> empty
// string.
auto LyraStringConcat(const LyraStringHandle* elems, int64_t count)
    -> LyraStringHandle;

// Convert packed bits to string (bytes from MSB to LSB, skip leading zeros).
// data: pointer to packed bits (little-endian bytes)
// bit_width: number of bits in packed value
// Returns owned handle (+1 refcount).
auto LyraStringFromPacked(const void* data, int32_t bit_width)
    -> LyraStringHandle;

// Convert string to packed bits (bytes packed MSB first).
// handle: input string
// out_data: output buffer for packed bits (little-endian bytes)
// bit_width: target bit width (determines output size)
// Pads with zeros on left if string is shorter, truncates rightmost if longer.
void LyraPackedFromString(
    LyraStringHandle handle, void* out_data, int32_t bit_width);

// Decrement refcount, free if 0. No-op for null.
void LyraStringRelease(LyraStringHandle handle);

// Print string contents to stdout (for $display and $fatal messages).
// Does NOT retain - reads immediately; handle must be valid for call duration.
// spec: pointer to format specification (width, alignment, etc.)
void LyraPrintString(
    LyraStringHandle handle, const lyra::runtime::LyraFormatSpec* spec);

// Get a non-owning view of string data (ptr + len).
// Does NOT transfer ownership. Returned pointer valid while handle is valid.
// out_ptr/out_len must not be null. If handle is null, sets *out_ptr="",
// *out_len=0.
void LyraStringGetView(
    LyraStringHandle handle, const char** out_ptr, uint64_t* out_len);

// Opaque buffer handle for string formatting (C ABI)
struct LyraStringFormatBuffer;

// Start a format buffer. Caller MUST call Finish to avoid leak.
auto LyraStringFormatStart() -> LyraStringFormatBuffer*;

// Append literal text (ptr+len, NOT NUL-terminated - supports embedded \0)
void LyraStringFormatLiteral(
    LyraStringFormatBuffer* buf, const char* str, int64_t len);

// Append formatted value (mirrors LyraPrintValue signature exactly)
// Note: x_mask/z_mask for future 4-state support (currently expected null)
void LyraStringFormatValue(
    LyraStringFormatBuffer* buf, int32_t format, int32_t value_kind,
    const void* data, int32_t width, bool is_signed, int32_t output_width,
    int32_t precision, bool zero_pad, bool left_align, const void* x_mask,
    const void* z_mask);

// Append string handle contents (FormatKind::kString path)
// Does NOT retain - reads immediately; handle must be valid for call duration
void LyraStringFormatString(
    LyraStringFormatBuffer* buf, LyraStringHandle handle);

// Finish and return new string handle (refcount=1). CONSUMES buffer.
auto LyraStringFormatFinish(LyraStringFormatBuffer* buf) -> LyraStringHandle;

// Format with runtime format string (for $sformat with variable format).
// Parameters:
//   format_handle: string handle containing format string
//   data_ptrs: array of pointers to operand data (or LyraStringHandle for
//   strings) widths: array of bit widths (32/64 for real/shortreal) signeds:
//   array of signedness flags (0=unsigned, 1=signed) kinds: array of value
//   kinds (0=integral, 1=real, 2=string) count: number of operands
// Returns: newly allocated string handle (refcount=1)
auto LyraStringFormatRuntime(
    LyraStringHandle format_handle, void* const* data_ptrs,
    const int32_t* widths, const int8_t* signeds, const int32_t* kinds,
    int64_t count) -> LyraStringHandle;
}

// C++ convenience wrapper: returns non-owning view of string data.
// If handle is null, returns empty string_view.
inline auto LyraStringAsView(LyraStringHandle handle) -> std::string_view {
  const char* ptr = nullptr;
  uint64_t len = 0;
  LyraStringGetView(handle, &ptr, &len);
  return {ptr, len};
}
