#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>
#include <vector>

#include "lyra/value/associative_array.hpp"
#include "lyra/value/dynamic_array.hpp"
#include "lyra/value/enumeration.hpp"
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
// what its caller hands it. Which numbers a word takes is its type's (LRM
// 21.4.2), so a load into a memory of an enumerated type is handed the
// enumeration's members as its own constant.

// What a load completes with: the memory it filled.
template <typename Memory>
using MemoryLoad = value::Tuple<Memory>;

// The radix a memory task's words are written in (LRM 21.4): binary for the
// `b` tasks, whose base is 2, and hexadecimal for the `h` ones.
[[nodiscard]] constexpr auto MemoryRadix(unsigned base) -> value::DigitRadix {
  return base == 2U ? value::DigitRadix::kBinary : value::DigitRadix::kHex;
}

// A word written out at full width in radix `base`, x and z per digit (LRM
// 21.5.1), as `%h` / `%b` display it.
[[nodiscard]] auto RenderedMemoryWord(
    const value::ConstIntegralView& word, unsigned base) -> std::string;

template <value::IntegralValue T>
[[nodiscard]] auto RenderedMemoryWord(const T& word, unsigned base)
    -> std::string {
  return RenderedMemoryWord(word.AsWords().View(), base);
}

// The values a word of a memory takes from its file (LRM 21.4.2): any number
// the file writes, or, in a memory of an enumerated type, a member's only.
struct AnyNumber {};
struct MemberValues {
  value::Enumeration members;
};
using WordValues = std::variant<AnyNumber, MemberValues>;

// Stores the word a token names at one grid coordinate. The token is already
// known to be a word the memory takes.
using StoreMemoryWord = std::function<void(
    std::int64_t address, std::size_t ordinal, std::string_view token)>;

// Stores the word a token names at one key, making the entry where there is
// none. The token is already known to be a word the memory takes.
using StoreKeyedWord =
    std::function<void(std::int64_t key, std::string_view token)>;

// The word at one grid coordinate, written out.
using RenderMemoryWord =
    std::function<std::string(std::int64_t address, std::size_t ordinal)>;

// LRM 21.4 / 21.4.3 load core over a rectangular address grid: `top_lo..top_hi`
// highest-dimension words, each expanding to `inner_count` leaves in row-major
// order, each stored through `store`. A one-dimensional memory is the
// `inner_count == 1` case; a multidimensional one passes its inner leaf span.
// An `@address` repositions the highest-dimension cursor and resets the inner
// ordinal, and a highest-dimension word the file leaves partly filled keeps
// its remaining leaves. A token that is no word of `values` is an error after
// which nothing more is read.
void ReadMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const WordValues& values, const StoreMemoryWord& store);

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
// at its key through `store`, and one that is no word of `values` ends the
// load as on the grid.
void ReadMemKeyedCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const WordValues& values, const StoreKeyedWord& store);

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
template <typename U, std::size_t N>
struct IsMemoryLevel<value::UnpackedArray<U, N>> : std::true_type {};

// The packed word a memory level of type `T` bottoms out at.
template <typename T>
struct MemoryWordOf {
  using Type = T;
};
template <typename U, std::size_t N>
struct MemoryWordOf<value::UnpackedArray<U, N>> {
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
        node.ElementMut(AddressPosition(range, address)), rest, linear % inner);
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

// Stores a token the memory takes as the word `slot` holds.
template <value::IntegralValue T>
void StoreParsed(T& slot, std::string_view token, unsigned base) {
  value::FromDigits(slot, MemoryRadix(base), token);
}

// A dynamic array or queue is a 0-based memory whose address range is
// `[0, size-1]` (LRM 21.4.1: the current size is fixed, not resized by the
// load). Both containers expose the same ordinal element API, so one template
// serves both.
template <typename Container>
void ReadMemZeroBased(
    RuntimeEffects& runtime, Container& dest, const value::String& filename,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish, const WordValues& values) {
  ReadMemGridCore(
      runtime, filename, base, 0, static_cast<std::int64_t>(dest.RawSize()) - 1,
      1, start, finish, values,
      [&dest, base](std::int64_t address, std::size_t, std::string_view token) {
        StoreParsed(
            dest.ElementMut(value::Position::FromInt(address)), token, base);
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
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const WordValues& values) {
  ReadMemKeyedCore(
      runtime, filename, base, start, finish, values,
      [&dest, base](std::int64_t key, std::string_view token) {
        StoreParsed(dest.ElementMut(K::FromInt(key)), token, base);
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

template <typename Inner, std::size_t N>
void ReadMemMultidim(
    RuntimeEffects& runtime, value::UnpackedArray<Inner, N>& dest,
    const value::String& filename, std::span<const value::UnpackedRange> dims,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish, const WordValues& values) {
  const value::UnpackedRange addressed = dims[0];
  const std::span<const value::UnpackedRange> inner = dims.subspan(1);
  ReadMemGridCore(
      runtime, filename, base, addressed.Low(), addressed.High(),
      InnerLeafCount(inner), start, finish, values,
      [&dest, addressed, inner, base](
          std::int64_t top, std::size_t ordinal, std::string_view token) {
        auto& slot = dest.ElementMut(AddressPosition(addressed, top));
        StoreParsed(LeafByLinearIndex(slot, inner, ordinal), token, base);
      });
}

template <typename Inner, std::size_t N>
void WriteMemMultidim(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner, N>& src,
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

// The load and the dump of each kind of memory. A run with no window is one
// with no finish.
template <value::IntegralValue T>
auto LoadMemory(
    RuntimeEffects& runtime, const value::DynamicArray<T>& memory,
    const WordValues& values, const value::String& filename, std::int64_t base,
    std::int64_t start, std::optional<std::int64_t> finish)
    -> MemoryLoad<value::DynamicArray<T>> {
  MemoryLoad<value::DynamicArray<T>> load(memory);
  detail::ReadMemZeroBased(
      runtime, load.template Component<0>(), filename,
      static_cast<unsigned>(base), start, finish, values);
  return load;
}

template <value::IntegralValue T>
void DumpMemory(
    RuntimeEffects& runtime, const value::DynamicArray<T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish) {
  detail::WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base), start, finish);
}

template <value::IntegralValue T, std::int64_t kBound>
auto LoadMemory(
    RuntimeEffects& runtime, const value::Queue<T, kBound>& memory,
    const WordValues& values, const value::String& filename, std::int64_t base,
    std::int64_t start, std::optional<std::int64_t> finish)
    -> MemoryLoad<value::Queue<T, kBound>> {
  MemoryLoad<value::Queue<T, kBound>> load(memory);
  detail::ReadMemZeroBased(
      runtime, load.template Component<0>(), filename,
      static_cast<unsigned>(base), start, finish, values);
  return load;
}

template <value::IntegralValue T, std::int64_t kBound>
void DumpMemory(
    RuntimeEffects& runtime, const value::Queue<T, kBound>& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish) {
  detail::WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base), start, finish);
}

template <value::IntegralValue K, value::IntegralValue T>
auto LoadMemory(
    RuntimeEffects& runtime, const value::AssociativeArray<K, T>& memory,
    const WordValues& values, const value::String& filename, std::int64_t base,
    std::int64_t start, std::optional<std::int64_t> finish)
    -> MemoryLoad<value::AssociativeArray<K, T>> {
  MemoryLoad<value::AssociativeArray<K, T>> load(memory);
  detail::ReadMemAssociative(
      runtime, load.template Component<0>(), filename,
      static_cast<unsigned>(base), start, finish, values);
  return load;
}

template <value::IntegralValue K, value::IntegralValue T>
void DumpMemory(
    RuntimeEffects& runtime, const value::AssociativeArray<K, T>& src,
    const value::String& filename, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish) {
  detail::WriteMemAssociative(
      runtime, src, filename, static_cast<unsigned>(base), start, finish);
}

// An unpacked memory of any depth. `bounds` states one declared range per
// dimension, the first being the addressed dimension and the rest describing
// the leaves each address expands to, so a one-dimensional memory is the
// one-element case of the same traversal.
template <typename Inner, std::size_t N>
  requires detail::IsMemoryLevel<Inner>::value
auto LoadMemory(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner, N>& memory,
    const WordValues& values, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish)
    -> MemoryLoad<value::UnpackedArray<Inner, N>> {
  MemoryLoad<value::UnpackedArray<Inner, N>> load(memory);
  detail::ReadMemMultidim(
      runtime, load.template Component<0>(), filename,
      value::UnpackedRangesOf(bounds), static_cast<unsigned>(base), start,
      finish, values);
  return load;
}

template <typename Inner, std::size_t N>
  requires detail::IsMemoryLevel<Inner>::value
void DumpMemory(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner, N>& src,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start, std::optional<std::int64_t> finish) {
  detail::WriteMemMultidim(
      runtime, src, filename, value::UnpackedRangesOf(bounds),
      static_cast<unsigned>(base), start, finish);
}

// The loads and dumps a call names: either addressing request (above), and for
// a load a memory of any type or of an enumerated one, which names the members
// its words are read as after the memory. Each reaches the load or the dump of
// the memory's own kind.
template <typename Memory, typename... Addressing>
auto ReadMem(
    RuntimeEffects& runtime, const Memory& memory,
    const value::String& filename, const Addressing&... addressing) {
  return LoadMemory(
      runtime, memory, WordValues{AnyNumber{}}, filename, addressing...,
      std::nullopt);
}

template <typename Memory, typename... Addressing>
auto ReadMemWithin(
    RuntimeEffects& runtime, const Memory& memory,
    const value::String& filename, const Addressing&... addressing) {
  return LoadMemory(
      runtime, memory, WordValues{AnyNumber{}}, filename, addressing...);
}

template <
    typename Memory, std::uint64_t kWidth, bool kFourState,
    std::size_t kMembers, typename... Addressing>
auto ReadMem(
    RuntimeEffects& runtime, const Memory& memory,
    const value::EnumerationMembers<kWidth, kFourState, kMembers>& members,
    const value::String& filename, const Addressing&... addressing) {
  return LoadMemory(
      runtime, memory, WordValues{MemberValues{members.Read()}}, filename,
      addressing..., std::nullopt);
}

template <
    typename Memory, std::uint64_t kWidth, bool kFourState,
    std::size_t kMembers, typename... Addressing>
auto ReadMemWithin(
    RuntimeEffects& runtime, const Memory& memory,
    const value::EnumerationMembers<kWidth, kFourState, kMembers>& members,
    const value::String& filename, const Addressing&... addressing) {
  return LoadMemory(
      runtime, memory, WordValues{MemberValues{members.Read()}}, filename,
      addressing...);
}

template <typename Memory, typename... Addressing>
void WriteMem(
    RuntimeEffects& runtime, const Memory& memory,
    const value::String& filename, const Addressing&... addressing) {
  DumpMemory(runtime, memory, filename, addressing..., std::nullopt);
}

template <typename Memory, typename... Addressing>
void WriteMemWithin(
    RuntimeEffects& runtime, const Memory& memory,
    const value::String& filename, const Addressing&... addressing) {
  DumpMemory(runtime, memory, filename, addressing...);
}

}  // namespace lyra::runtime
