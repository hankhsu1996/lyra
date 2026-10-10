#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/value/associative_array.hpp"
#include "lyra/value/dynamic_array.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/unpacked_range.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// LRM 21.4 $readmemh / $readmemb. Loads a memory from the text file named
// `filename`: whitespace-separated radix-`base` words (base 16 for $readmemh, 2
// for $readmemb), optional `@hex` address directives, `//` and `/* */`
// comments, and per-digit x / z / ?. Each word is written by declared index, so
// a descending or non-zero-based memory resolves correctly, and a word the file
// does not address keeps what it held, which is why the memory crosses in and
// rides the completion back out.
//
// LRM 21.5 $writememh / $writememb dumps the same memory in a form the load
// reads back: one radix-`base` word per line, each rendered at full width with
// per-digit x / z. An existing file is overwritten (no append).
//
// Addressing is one of two requests, and the caller says which. The plain form
// runs upward from `start`, which the caller materializes as the memory's
// lowest address where the source named none -- the clause's no-address form
// and its start-only form are the same run. The windowed form names a `finish`
// as well, which bounds the range an `@address` may reach, lets the run
// descend, and obliges the file to fill the whole window (LRM 21.4).
//
// The memory itself decides what an address means: an unpacked array reads its
// declared bounds, a dynamic array or queue is the 0-based dense space its
// current size spans, and an associative array is addressed by key, built at
// its index type so it compares equal to the key an ordinary access builds.
// The element is a single packed vector in every one of them.
//
// The file, its tokens, the addressing and the diagnostics are compiled once,
// in the cores below. What a word is -- how a token becomes one, and how one
// is written out -- is the holder's, so each core reaches the words through
// what its caller hands it.

// What a load completes with: the memory it filled.
template <typename Memory>
using MemoryLoad = value::Tuple<Memory>;

// The radix a memory task's words are written in (LRM 21.4): binary for the
// `b` tasks, whose base is 2, and hexadecimal for the `h` ones.
[[nodiscard]] constexpr auto MemoryRadix(unsigned base) -> value::DigitRadix {
  return base == 2U ? value::DigitRadix::kBinary : value::DigitRadix::kHex;
}

// A token read as a word of `T`, at radix `base`; none where it is not one.
template <value::IntegralValue T>
[[nodiscard]] auto ParsedMemoryWord(std::string_view token, unsigned base)
    -> std::optional<T> {
  typename T::Words words;
  if (!value::FromDigits(words.Write(), T::kWidth, MemoryRadix(base), token)) {
    return std::nullopt;
  }
  return T::FromWords(words);
}

// A word written out at full width in radix `base`, x and z per digit (LRM
// 21.5.1), as `%h` / `%b` display it.
[[nodiscard]] auto RenderedMemoryWord(
    const value::ConstIntegralView& word, unsigned base) -> std::string;

template <value::IntegralValue T>
[[nodiscard]] auto RenderedMemoryWord(const T& word, unsigned base)
    -> std::string {
  return RenderedMemoryWord(word.Load().View(), base);
}

// Stores the word a token names at one grid coordinate, answering whether the
// token is a word at all.
using StoreMemoryWord = std::function<bool(
    std::int64_t address, std::size_t ordinal, std::string_view token)>;

// The word at one grid coordinate, written out.
using RenderMemoryWord =
    std::function<std::string(std::int64_t address, std::size_t ordinal)>;

// LRM 21.4 / 21.4.3 load core over a rectangular address grid: `top_lo..top_hi`
// highest-dimension words, each expanding to `inner_count` leaves in row-major
// order, each stored through `store`. A one-dimensional memory is the
// `inner_count == 1` case; a multidimensional one passes its inner leaf span.
// An `@address` repositions the highest-dimension cursor and resets the inner
// ordinal, and a highest-dimension word the file leaves partly filled keeps
// its remaining leaves.
void ReadMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const StoreMemoryWord& store);

// LRM 21.5 dump core over the same grid. Writes every leaf in ascending-address
// row-major order; no `@address` is written (that is the associative dump's
// job).
void WriteMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const RenderMemoryWord& rendered);

// LRM 21.4.1 associative load. Addressing is by key: an `@key` sets the
// cursor, and consecutive words advance it. `start` / `finish` bound the key
// range; without them the keys come entirely from the file. A word is stored
// at its key through `store`, which makes the entry where there is none.
void ReadMemKeyedCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const std::function<bool(std::int64_t key, std::string_view token)>& store);

// One entry of an associative memory as a dump writes it: its key, and the key
// and the word written out.
struct RenderedMemoryEntry {
  std::int64_t key = 0;
  std::string key_text;
  std::string word_text;
};

// LRM 21.5.3 associative dump. The entries arrive in ascending key order, each
// written as an `@key` line followed by the word, so a sparse array
// round-trips through `$readmem`. `start` / `finish` bound the key range when
// supplied.
void WriteMemKeyedCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    std::span<const RenderedMemoryEntry> entries);

namespace detail {

// A level of a memory: an array of the level below, or the packed word the
// nesting bottoms out at. A one-dimensional memory is the depth-one case of
// the same traversal, so it needs no form of its own.
template <typename T>
struct IsMemoryLevel : std::bool_constant<value::IntegralValue<T>> {};
template <typename U>
struct IsMemoryLevel<value::UnpackedArray<U>> : std::true_type {};

// The packed word a memory level of type `T` bottoms out at.
template <typename T>
struct MemoryWordOf {
  using Type = T;
};
template <typename U>
struct MemoryWordOf<value::UnpackedArray<U>> {
  using Type = typename MemoryWordOf<U>::Type;
};

// Leaf count of one highest-dimension word: the product of the inner dimension
// sizes. No inner dimension (the highest is itself the leaf level) is one leaf,
// which is the empty product rather than a case of its own.
[[nodiscard]] inline auto InnerLeafCount(
    std::span<const value::UnpackedRange> dims) -> std::size_t {
  std::size_t count = 1;
  for (const value::UnpackedRange& dim : dims) {
    count *= dim.Count();
  }
  return count;
}

// The position a memory address names in one level. A memory file states its
// addresses in the coordinates the memory was declared with (LRM 21.4), so this
// is where they are read against the declared range.
[[nodiscard]] inline auto AddressPosition(
    const value::UnpackedRange& range, std::int64_t address)
    -> value::Position {
  return value::Position::FromInt(range.ToOrdinal(address));
}

// Resolves a row-major leaf ordinal within one subtree to its storage cell,
// mapping each dimension's ascending-address position through its declared
// range so a descending declaration still reads low address first. The load
// path takes a mutable cell and the dump path a const one; the ordinal decode
// is identical.
template <typename T>
[[nodiscard]] auto LeafByLinearIndex(
    T& node, std::span<const value::UnpackedRange> dims, std::size_t linear) ->
    typename MemoryWordOf<T>::Type& {
  if constexpr (value::IntegralValue<T>) {
    return node;
  } else {
    const value::UnpackedRange& range = dims[0];
    const std::span<const value::UnpackedRange> rest = dims.subspan(1);
    const std::size_t inner = InnerLeafCount(rest);
    const std::int64_t address =
        range.Low() + static_cast<std::int64_t>(linear / inner);
    return LeafByLinearIndex(
        node.ElementRef(AddressPosition(range, address)), rest, linear % inner);
  }
}

template <typename T>
[[nodiscard]] auto LeafByLinearIndexConst(
    const T& node, std::span<const value::UnpackedRange> dims,
    std::size_t linear) -> const typename MemoryWordOf<T>::Type& {
  if constexpr (value::IntegralValue<T>) {
    return node;
  } else {
    const value::UnpackedRange& range = dims[0];
    const std::span<const value::UnpackedRange> rest = dims.subspan(1);
    const std::size_t inner = InnerLeafCount(rest);
    const std::int64_t address =
        range.Low() + static_cast<std::int64_t>(linear / inner);
    return LeafByLinearIndexConst(
        node.Element(AddressPosition(range, address)), rest, linear % inner);
  }
}

// Stores a token as the word `slot` holds, answering whether it is one.
template <value::IntegralValue T>
[[nodiscard]] auto StoreParsed(T& slot, std::string_view token, unsigned base)
    -> bool {
  const std::optional<T> word = ParsedMemoryWord<T>(token, base);
  if (!word) {
    return false;
  }
  slot = *word;
  return true;
}

// A dynamic array or queue is a 0-based memory whose address range is
// `[0, size-1]` (LRM 21.4.1: the current size is fixed, not resized by the
// load). Both containers expose the same ordinal element API, so one template
// serves both.
template <typename Container>
void ReadMemZeroBased(
    RuntimeEffects& runtime, Container& dest, const value::String& filename,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  ReadMemGridCore(
      runtime, filename, base, 0, static_cast<std::int64_t>(dest.RawSize()) - 1,
      1, start, finish,
      [&dest, base](std::int64_t address, std::size_t, std::string_view token) {
        return StoreParsed(
            dest.ElementRef(value::Position::FromInt(address)), token, base);
      });
}

template <typename Container>
void WriteMemZeroBased(
    RuntimeEffects& runtime, const Container& src,
    const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish) {
  WriteMemGridCore(
      runtime, filename, base, 0, static_cast<std::int64_t>(src.RawSize()) - 1,
      1, start, finish, [&src, base](std::int64_t address, std::size_t) {
        return RenderedMemoryWord(
            src.RawAt(static_cast<std::size_t>(address)), base);
      });
}

template <value::IntegralValue K, value::IntegralValue T>
void ReadMemAssociative(
    RuntimeEffects& runtime, value::AssociativeArray<K, T>& dest,
    const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish) {
  ReadMemKeyedCore(
      runtime, filename, base, start, finish,
      [&dest, base](std::int64_t key, std::string_view token) {
        return StoreParsed(dest.ElementRef(K::FromInt(key)), token, base);
      });
}

template <value::IntegralValue K, value::IntegralValue T>
void WriteMemAssociative(
    RuntimeEffects& runtime, const value::AssociativeArray<K, T>& src,
    const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish) {
  std::vector<RenderedMemoryEntry> entries;
  src.ForEachEntry([&entries, base](const K& key, const T& word) {
    entries.push_back(
        RenderedMemoryEntry{
            .key = key.ToInt64(),
            .key_text = RenderedMemoryWord(key, 16U),
            .word_text = RenderedMemoryWord(word, base)});
  });
  WriteMemKeyedCore(runtime, filename, base, start, finish, entries);
}

template <typename Inner>
void ReadMemMultidim(
    RuntimeEffects& runtime, value::UnpackedArray<Inner>& dest,
    const value::String& filename, std::span<const value::UnpackedRange> dims,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  const value::UnpackedRange addressed = dims[0];
  const std::span<const value::UnpackedRange> inner = dims.subspan(1);
  ReadMemGridCore(
      runtime, filename, base, addressed.Low(), addressed.High(),
      InnerLeafCount(inner), start, finish,
      [&dest, addressed, inner, base](
          std::int64_t top, std::size_t ordinal, std::string_view token) {
        auto& slot = dest.ElementRef(AddressPosition(addressed, top));
        return StoreParsed(
            LeafByLinearIndex(slot, inner, ordinal), token, base);
      });
}

template <typename Inner>
void WriteMemMultidim(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::UnpackedRange> dims,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  const value::UnpackedRange addressed = dims[0];
  const std::span<const value::UnpackedRange> inner = dims.subspan(1);
  WriteMemGridCore(
      runtime, filename, base, addressed.Low(), addressed.High(),
      InnerLeafCount(inner), start, finish,
      [&src, addressed, inner, base](std::int64_t top, std::size_t ordinal) {
        const auto& slot = src.Element(AddressPosition(addressed, top));
        return RenderedMemoryWord(
            LeafByLinearIndexConst(slot, inner, ordinal), base);
      });
}

}  // namespace detail

template <value::IntegralValue T>
auto ReadMem(
    RuntimeEffects& runtime, value::DynamicArray<T> dest,
    const value::String& filename, std::int64_t base, std::int64_t start)
    -> MemoryLoad<value::DynamicArray<T>> {
  detail::ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base), start,
      std::nullopt);
  return MemoryLoad<value::DynamicArray<T>>{std::move(dest)};
}

template <value::IntegralValue T>
auto ReadMemWithin(
    RuntimeEffects& runtime, value::DynamicArray<T> dest,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryLoad<value::DynamicArray<T>> {
  detail::ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base), start, finish);
  return MemoryLoad<value::DynamicArray<T>>{std::move(dest)};
}

template <value::IntegralValue T>
void WriteMem(
    RuntimeEffects& runtime, const value::DynamicArray<T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start) {
  detail::WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base), start, std::nullopt);
}

template <value::IntegralValue T>
void WriteMemWithin(
    RuntimeEffects& runtime, const value::DynamicArray<T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::int64_t finish) {
  detail::WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base), start, finish);
}

template <value::IntegralValue T>
auto ReadMem(
    RuntimeEffects& runtime, value::Queue<T> dest,
    const value::String& filename, std::int64_t base, std::int64_t start)
    -> MemoryLoad<value::Queue<T>> {
  detail::ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base), start,
      std::nullopt);
  return MemoryLoad<value::Queue<T>>{std::move(dest)};
}

template <value::IntegralValue T>
auto ReadMemWithin(
    RuntimeEffects& runtime, value::Queue<T> dest,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryLoad<value::Queue<T>> {
  detail::ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base), start, finish);
  return MemoryLoad<value::Queue<T>>{std::move(dest)};
}

template <value::IntegralValue T>
void WriteMem(
    RuntimeEffects& runtime, const value::Queue<T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start) {
  detail::WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base), start, std::nullopt);
}

template <value::IntegralValue T>
void WriteMemWithin(
    RuntimeEffects& runtime, const value::Queue<T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::int64_t finish) {
  detail::WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base), start, finish);
}

template <value::IntegralValue K, value::IntegralValue T>
auto ReadMem(
    RuntimeEffects& runtime, value::AssociativeArray<K, T> dest,
    const value::String& filename, std::int64_t base, std::int64_t start)
    -> MemoryLoad<value::AssociativeArray<K, T>> {
  detail::ReadMemAssociative(
      runtime, dest, filename, static_cast<unsigned>(base), start,
      std::nullopt);
  return MemoryLoad<value::AssociativeArray<K, T>>{std::move(dest)};
}

template <value::IntegralValue K, value::IntegralValue T>
auto ReadMemWithin(
    RuntimeEffects& runtime, value::AssociativeArray<K, T> dest,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryLoad<value::AssociativeArray<K, T>> {
  detail::ReadMemAssociative(
      runtime, dest, filename, static_cast<unsigned>(base), start, finish);
  return MemoryLoad<value::AssociativeArray<K, T>>{std::move(dest)};
}

template <value::IntegralValue K, value::IntegralValue T>
void WriteMem(
    RuntimeEffects& runtime, const value::AssociativeArray<K, T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start) {
  detail::WriteMemAssociative(
      runtime, src, filename, static_cast<unsigned>(base), start, std::nullopt);
}

template <value::IntegralValue K, value::IntegralValue T>
void WriteMemWithin(
    RuntimeEffects& runtime, const value::AssociativeArray<K, T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::int64_t finish) {
  detail::WriteMemAssociative(
      runtime, src, filename, static_cast<unsigned>(base), start, finish);
}

// An unpacked memory of any depth. `bounds` states one declared range per
// dimension, the first being the addressed dimension and the rest describing
// the leaves each address expands to, so a one-dimensional memory is the
// one-element case of the same traversal.
template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
auto ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<Inner> dest,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start)
    -> MemoryLoad<value::UnpackedArray<Inner>> {
  detail::ReadMemMultidim(
      runtime, dest, filename, value::UnpackedRangesOf(bounds),
      static_cast<unsigned>(base), start, std::nullopt);
  return MemoryLoad<value::UnpackedArray<Inner>>{std::move(dest)};
}

template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
auto ReadMemWithin(
    RuntimeEffects& runtime, value::UnpackedArray<Inner> dest,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start, std::int64_t finish)
    -> MemoryLoad<value::UnpackedArray<Inner>> {
  detail::ReadMemMultidim(
      runtime, dest, filename, value::UnpackedRangesOf(bounds),
      static_cast<unsigned>(base), start, finish);
  return MemoryLoad<value::UnpackedArray<Inner>>{std::move(dest)};
}

template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
void WriteMem(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start) {
  detail::WriteMemMultidim(
      runtime, src, filename, value::UnpackedRangesOf(bounds),
      static_cast<unsigned>(base), start, std::nullopt);
}

template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
void WriteMemWithin(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start, std::int64_t finish) {
  detail::WriteMemMultidim(
      runtime, src, filename, value::UnpackedRangesOf(bounds),
      static_cast<unsigned>(base), start, finish);
}

}  // namespace lyra::runtime
