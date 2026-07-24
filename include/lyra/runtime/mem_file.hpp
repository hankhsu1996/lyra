#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <type_traits>

#include "lyra/value/associative_array.hpp"
#include "lyra/value/dynamic_array.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// LRM 21.4 $readmemh / $readmemb. Loads the memory `dest` from the text file
// named `filename`: whitespace-separated radix-`base` words (base 16 for
// $readmemh, 2 for $readmemb), optional `@hex` address directives, `//` and
// `/* */` comments, and per-digit x / z / ?. `declared_left` / `declared_right`
// are dest's declared index bounds; each word is written by declared index, so
// a descending or non-zero-based memory resolves correctly. Words the file does
// not address keep their prior value. `dest` is the lowering's copy-out temp,
// committed to the SV memory after the call returns.
//
// Addressing (LRM 21.4): the no-range form fills from the lowest declared index
// upward; the `start`-only form fills upward from `start`; the `start`/`finish`
// form fills from `start` toward `finish`, descending when `start > finish`. An
// `@address` in the file repositions the write cursor and must fall inside the
// active range, else the load stops with an error.
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base);
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start);
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

// LRM 21.5 $writememh / $writememb. Dumps the memory `src` to the text file
// named `filename` in a form `$readmem{h,b}` reads back: one radix-`base` word
// per line (base 16 for $writememh, 2 for $writememb), each element rendered at
// full width with per-digit x / z. `declared_left` / `declared_right` are src's
// declared index bounds. An existing file is overwritten (LRM 21.5, no append).
//
// Addressing (LRM 21.5.3): for an unpacked array no `@address` is written. The
// no-range form dumps the whole memory from the lowest declared index upward;
// the `start`-only form dumps upward from `start`; the `start`/`finish` form
// dumps from `start` toward `finish`, descending when `start > finish`.
void WriteMem(
    RuntimeEffects& runtime,
    const value::UnpackedArray<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base);
void WriteMem(
    RuntimeEffects& runtime,
    const value::UnpackedArray<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMem(
    RuntimeEffects& runtime,
    const value::UnpackedArray<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

// LRM 21.4.1 / 21.5: a dynamic array or queue memory. It is a 0-based dense
// space whose address range is `[0, size-1]`; the load does not resize it, so
// it carries no declared bounds -- the container's current size is the range.
// The element must still be a single packed vector.
void ReadMem(
    RuntimeEffects& runtime, value::DynamicArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& base);
void ReadMem(
    RuntimeEffects& runtime, value::DynamicArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void ReadMem(
    RuntimeEffects& runtime, value::DynamicArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);
void WriteMem(
    RuntimeEffects& runtime, const value::DynamicArray<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base);
void WriteMem(
    RuntimeEffects& runtime, const value::DynamicArray<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMem(
    RuntimeEffects& runtime, const value::DynamicArray<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

void ReadMem(
    RuntimeEffects& runtime, value::Queue<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& base);
void ReadMem(
    RuntimeEffects& runtime, value::Queue<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void ReadMem(
    RuntimeEffects& runtime, value::Queue<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);
void WriteMem(
    RuntimeEffects& runtime, const value::Queue<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base);
void WriteMem(
    RuntimeEffects& runtime, const value::Queue<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMem(
    RuntimeEffects& runtime, const value::Queue<value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

// LRM 21.4.1 / 21.5.3: an associative memory with an integral index. It is
// addressed by key, so it carries no declared range; a load creates the entries
// the file addresses, and a dump writes one `@key` line per entry in ascending
// key order. A load takes a `key_prototype` -- a default value of the declared
// index type -- so it builds each key at the width an ordinary `mem[i]` access
// uses (the map orders keys of one common width).
void ReadMem(
    RuntimeEffects& runtime,
    value::AssociativeArray<value::PackedArray, value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base);
void ReadMem(
    RuntimeEffects& runtime,
    value::AssociativeArray<value::PackedArray, value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start);
void ReadMem(
    RuntimeEffects& runtime,
    value::AssociativeArray<value::PackedArray, value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish);
void WriteMem(
    RuntimeEffects& runtime,
    const value::AssociativeArray<value::PackedArray, value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base);
void WriteMem(
    RuntimeEffects& runtime,
    const value::AssociativeArray<value::PackedArray, value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMem(
    RuntimeEffects& runtime,
    const value::AssociativeArray<value::PackedArray, value::PackedArray>& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

// LRM 21.4.3: multidimensional memory support. The type-driven navigation lives
// in these header templates -- arbitrary nesting depth, traversed in ascending
// address at every dimension (the file is address-ordered even when a dimension
// is declared descending) -- while the file / parse / render / diagnostic logic
// stays in the non-template cores below, reached through a leaf accessor. The
// dimensions ride as a `[left0, right0, left1, right1, ...]` bounds array: the
// first pair is the highest (addressed) dimension, the rest describe the leaves
// each highest-dimension word expands to, row-major.
namespace detail {

template <typename T>
struct IsNestedArray : std::false_type {};
template <typename U>
struct IsNestedArray<value::UnpackedArray<U>> : std::true_type {};

// Leaf count of one highest-dimension word: the product of the inner dimension
// sizes. An empty bounds list (the highest dimension is itself the leaf level)
// is one leaf.
[[nodiscard]] inline auto InnerLeafCount(
    std::span<const value::PackedArray> dims) -> std::size_t {
  std::size_t count = 1;
  for (std::size_t i = 0; i + 1 < dims.size(); i += 2) {
    const std::int64_t left = dims[i].ToInt64();
    const std::int64_t right = dims[i + 1].ToInt64();
    count *= static_cast<std::size_t>(
        (left >= right ? left - right : right - left) + 1);
  }
  return count;
}

// Resolves a row-major leaf ordinal within one subtree to its storage cell,
// mapping each dimension's ascending-address position through the declared
// `[left, right]` so a descending declaration still reads low address first.
// The load path takes a mutable cell (`ElementRef`); the dump path reads a
// const cell (`Element`); the ordinal decode is identical.
template <typename T>
[[nodiscard]] auto LeafByLinearIndex(
    T& node, std::span<const value::PackedArray> dims, std::size_t linear)
    -> value::PackedArray& {
  if constexpr (std::is_same_v<T, value::PackedArray>) {
    return node;
  } else {
    const std::int64_t left = dims[0].ToInt64();
    const std::int64_t right = dims[1].ToInt64();
    const std::span<const value::PackedArray> rest = dims.subspan(2);
    const std::size_t inner = InnerLeafCount(rest);
    const std::int64_t address =
        std::min(left, right) + static_cast<std::int64_t>(linear / inner);
    return LeafByLinearIndex(
        node.ElementRef(
            value::PackedArray::Int(static_cast<std::int32_t>(address)),
            dims[0], dims[1]),
        rest, linear % inner);
  }
}

template <typename T>
[[nodiscard]] auto LeafByLinearIndexConst(
    const T& node, std::span<const value::PackedArray> dims, std::size_t linear)
    -> const value::PackedArray& {
  if constexpr (std::is_same_v<T, value::PackedArray>) {
    return node;
  } else {
    const std::int64_t left = dims[0].ToInt64();
    const std::int64_t right = dims[1].ToInt64();
    const std::span<const value::PackedArray> rest = dims.subspan(2);
    const std::size_t inner = InnerLeafCount(rest);
    const std::int64_t address =
        std::min(left, right) + static_cast<std::int64_t>(linear / inner);
    return LeafByLinearIndexConst(
        node.Element(
            value::PackedArray::Int(static_cast<std::int32_t>(address)),
            dims[0], dims[1]),
        rest, linear % inner);
  }
}

}  // namespace detail

// LRM 21.4 / 21.4.3 load core over a rectangular address grid: `top_lo..top_hi`
// highest-dimension words, each expanding to `inner_count` leaves in row-major
// order, reached through `leaf_at(highest_address, inner_ordinal)`. A
// one-dimensional memory is the `inner_count == 1` case; a multidimensional one
// passes its inner leaf span. An `@address` repositions the highest-dimension
// cursor and resets the inner ordinal, and a highest-dimension word the file
// leaves partly filled keeps its remaining leaves.
void ReadMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const std::function<value::PackedArray&(std::int64_t, std::size_t)>&
        leaf_at);

// LRM 21.5 dump core over the same grid. Writes every leaf in ascending-address
// row-major order; no `@address` is written (that is the associative dump's
// job).
void WriteMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const std::function<const value::PackedArray&(std::int64_t, std::size_t)>&
        leaf_get);

template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void ReadMemMultidim(
    RuntimeEffects& runtime, value::UnpackedArray<Inner>& dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  const std::int64_t top_left = dims[0].ToInt64();
  const std::int64_t top_right = dims[1].ToInt64();
  const std::span<const value::PackedArray> inner = dims.subspan(2);
  ReadMemGridCore(
      runtime, filename, base, std::min(top_left, top_right),
      std::max(top_left, top_right), detail::InnerLeafCount(inner), start,
      finish,
      [&dest, dims, inner](
          std::int64_t top, std::size_t ordinal) -> value::PackedArray& {
        auto& slot = dest.ElementRef(
            value::PackedArray::Int(static_cast<std::int32_t>(top)), dims[0],
            dims[1]);
        return detail::LeafByLinearIndex(slot, inner, ordinal);
      });
}

template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void WriteMemMultidim(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  const std::int64_t top_left = dims[0].ToInt64();
  const std::int64_t top_right = dims[1].ToInt64();
  const std::span<const value::PackedArray> inner = dims.subspan(2);
  WriteMemGridCore(
      runtime, filename, base, std::min(top_left, top_right),
      std::max(top_left, top_right), detail::InnerLeafCount(inner), start,
      finish,
      [&src, dims, inner](
          std::int64_t top, std::size_t ordinal) -> const value::PackedArray& {
        const auto& slot = src.Element(
            value::PackedArray::Int(static_cast<std::int32_t>(top)), dims[0],
            dims[1]);
        return detail::LeafByLinearIndexConst(slot, inner, ordinal);
      });
}

// A multidimensional memory reaches the backend as an `UnpackedArray` whose
// element is itself an `UnpackedArray`; these overloads are selected by that
// nesting and forward to the depth-generic cores above. The dimension bounds
// arrive as one `std::array` (rendered from a bounds literal) that binds to the
// `std::span` the cores take.
template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<Inner>& dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base) {
  ReadMemMultidim(
      runtime, dest, filename, dims, static_cast<unsigned>(base.ToInt64()),
      std::nullopt, std::nullopt);
}
template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<Inner>& dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start) {
  ReadMemMultidim(
      runtime, dest, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}
template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<Inner>& dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) {
  ReadMemMultidim(
      runtime, dest, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void WriteMem(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base) {
  WriteMemMultidim(
      runtime, src, filename, dims, static_cast<unsigned>(base.ToInt64()),
      std::nullopt, std::nullopt);
}
template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void WriteMem(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start) {
  WriteMemMultidim(
      runtime, src, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}
template <typename Inner>
  requires detail::IsNestedArray<Inner>::value
void WriteMem(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) {
  WriteMemMultidim(
      runtime, src, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

}  // namespace lyra::runtime
