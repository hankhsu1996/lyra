#include "lyra/value/runtime_memory.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/runtime_value.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

namespace {

// The storage position an address names at one level, which is where the
// declared range is read (LRM 7.4.5). A bounds list describes the value it came
// from, so an address the range does not name is a caller defect.
auto PositionOf(
    const RuntimeUnpackedArray& level, std::int64_t address,
    const UnpackedRange& range) -> std::size_t {
  const auto size = static_cast<std::size_t>(level.Size().ToInt64());
  const std::optional<std::size_t> position = ResolveUnpackedOrdinal(
      PackedArray::Int(static_cast<std::int32_t>(address)), range, size);
  if (!position) {
    throw InternalError(
        "memory walk: the bounds name an address the memory does not hold");
  }
  return *position;
}

// The level below the one an address named, where the bounds say the nesting
// goes further.
auto LevelOf(const RuntimeValue& value) -> const RuntimeUnpackedArray& {
  const auto* level = std::get_if<RuntimeUnpackedArray>(&value.value);
  if (level == nullptr) {
    throw InternalError(
        "memory walk: the bounds describe more dimensions than the memory has");
  }
  return *level;
}

void CollectLevel(
    const RuntimeUnpackedArray& level, std::span<const UnpackedRange> dims,
    std::vector<PackedArray>& out) {
  const UnpackedRange& range = dims[0];
  const std::span<const UnpackedRange> inner = dims.subspan(1);
  for (std::int64_t address = range.Low(); address <= range.High(); ++address) {
    const RuntimeValue& element =
        level.ElementAt(PositionOf(level, address, range));
    if (inner.empty()) {
      out.push_back(MemoryWordOf(element));
    } else {
      CollectLevel(LevelOf(element), inner, out);
    }
  }
}

auto RebuildLevel(
    const RuntimeUnpackedArray& level, std::span<const UnpackedRange> dims,
    std::span<const PackedArray> words, std::size_t& cursor)
    -> RuntimeUnpackedArray {
  const UnpackedRange& range = dims[0];
  const std::span<const UnpackedRange> inner = dims.subspan(1);
  const auto size = static_cast<std::size_t>(level.Size().ToInt64());
  std::vector<RuntimeValue> elements;
  elements.reserve(size);
  for (std::size_t position = 0; position < size; ++position) {
    elements.push_back(level.ElementAt(position));
  }
  for (std::int64_t address = range.Low(); address <= range.High(); ++address) {
    const std::size_t position = PositionOf(level, address, range);
    if (inner.empty()) {
      if (cursor >= words.size()) {
        throw InternalError("memory walk: the words run out before the memory");
      }
      elements[position] = RuntimeValue{words[cursor]};
      ++cursor;
    } else {
      elements[position] = RuntimeValue{
          RebuildLevel(LevelOf(elements[position]), inner, words, cursor)};
    }
  }
  return {level.ElementDefault(), std::move(elements), 1};
}

}  // namespace

auto MemoryWordOf(const RuntimeValue& element) -> const PackedArray& {
  const auto* word = std::get_if<PackedArray>(&element.value);
  if (word == nullptr) {
    throw InternalError("memory: an element is a packed word");
  }
  return *word;
}

auto MemoryWords(
    const RuntimeUnpackedArray& memory, std::span<const UnpackedRange> dims)
    -> std::vector<PackedArray> {
  std::vector<PackedArray> words;
  CollectLevel(memory, dims, words);
  return words;
}

auto MemoryWithWords(
    const RuntimeUnpackedArray& memory, std::span<const UnpackedRange> dims,
    std::span<const PackedArray> words) -> RuntimeUnpackedArray {
  std::size_t cursor = 0;
  return RebuildLevel(memory, dims, words, cursor);
}

}  // namespace lyra::value
