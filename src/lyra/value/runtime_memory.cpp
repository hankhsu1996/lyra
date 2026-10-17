#include "lyra/value/runtime_memory.hpp"

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/fixed_array_value.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/unpacked_range.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

auto WordType(const ValueType& type) -> const IntegralValueType& {
  const IntegralValueType* word = type.AsIntegral();
  if (word == nullptr) {
    throw InternalError("memory: an element is a packed word");
  }
  return *word;
}

// The storage position an address names in a level of `count` parts, which is
// where the declared range is read (LRM 7.4.5). A bounds list describes the
// value it came from, so an address the range does not name is a caller
// defect.
auto PositionOf(
    std::size_t count, std::int64_t address, const UnpackedRange& range)
    -> std::size_t {
  const std::int64_t ordinal = range.ToOrdinal(address);
  if (ordinal < 0 || ordinal >= static_cast<std::int64_t>(count)) {
    throw InternalError(
        "memory walk: the bounds name an address the memory does not hold");
  }
  return static_cast<std::size_t>(ordinal);
}

// The address the leaf `ordinal` of one top address has in dimension `d`: the
// leaves an address expands to run in ascending address at every lower
// dimension, the last fastest (LRM 21.4.3).
auto InnerAddress(
    std::span<const UnpackedRange> dims, std::size_t d, std::size_t ordinal)
    -> std::int64_t {
  std::size_t below = 1;
  for (const UnpackedRange& lower : dims.subspan(d + 1)) {
    below *= lower.Count();
  }
  return dims[d].Low() +
         static_cast<std::int64_t>((ordinal / below) % dims[d].Count());
}

// The leaf at one grid coordinate, each level reached through its type's
// ordered parts, one level per declared dimension.
template <typename Level>
auto LeafAt(
    Level memory, const ValueType& memory_type,
    std::span<const UnpackedRange> dims, std::int64_t top, std::size_t ordinal)
    -> decltype(auto) {
  Level level = memory;
  const ValueType* type = &memory_type;
  for (std::size_t d = 0; d < dims.size(); ++d) {
    const std::int64_t address = d == 0 ? top : InnerAddress(dims, d, ordinal);
    const ValueType& part = type->PartType(level);
    level = type->PartAt(
        level, PositionOf(type->PartCount(level), address, dims[d]));
    type = &part;
  }
  return MemoryWordOf(*type, level);
}

}  // namespace

auto MemoryWordOf(const ValueType& type, void* element) -> MemoryWord {
  return MemoryWord{.type = &WordType(type), .bytes = element};
}

auto MemoryWordOf(const ValueType& type, const void* element)
    -> ConstMemoryWord {
  return ConstMemoryWord{.type = &WordType(type), .bytes = element};
}

auto MemoryLeaf(
    LaidOutValue& memory, std::span<const UnpackedRange> dims, std::int64_t top,
    std::size_t ordinal) -> MemoryWord {
  return LeafAt(memory.Bytes(), memory.Type(), dims, top, ordinal);
}

auto MemoryLeaf(
    const FixedArrayView& memory, std::span<const UnpackedRange> dims,
    std::int64_t top, std::size_t ordinal) -> ConstMemoryWord {
  return LeafAt(memory.bytes, memory.type->Type(), dims, top, ordinal);
}

}  // namespace lyra::value
