#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/unpacked_range.hpp"
#include "lyra/value/value_type.hpp"

// An unpacked memory of any nesting depth, read and written through the
// coordinates its declaration names. A memory task addresses words in ascending
// address at every dimension, row-major (LRM 21.4.3), and the library's memory
// carries its depth as a run-time fact -- so the bounds the declaration
// supplies are the whole of what drives the walk.
namespace lyra::value {

// One word of a memory where it lies, with the integral type it is of.
struct MemoryWord {
  const IntegralValueType* type;
  void* bytes;
};
struct ConstMemoryWord {
  const IntegralValueType* type;
  const void* bytes;
};

// The word an element of `type` lying at `element` holds. Every memory's
// element is a packed vector (LRM 21.4.1 / 21.5.1) and the front end rejects
// anything else, so an element of another type is a compiler bug.
[[nodiscard]] auto MemoryWordOf(const ValueType& type, const void* element)
    -> ConstMemoryWord;
[[nodiscard]] auto MemoryWordOf(const ValueType& type, void* element)
    -> MemoryWord;

// The word at one grid coordinate of `memory`, where it lies: `top` is an
// address of the addressed dimension, `dims[0]`, and `ordinal` counts the
// leaves that address expands to across the rest of `dims`, one declared range
// per dimension.
[[nodiscard]] auto MemoryLeaf(
    RuntimeUnpackedArray& memory, std::span<const UnpackedRange> dims,
    std::int64_t top, std::size_t ordinal) -> MemoryWord;
[[nodiscard]] auto MemoryLeaf(
    const RuntimeUnpackedArray& memory, std::span<const UnpackedRange> dims,
    std::int64_t top, std::size_t ordinal) -> ConstMemoryWord;

}  // namespace lyra::value
