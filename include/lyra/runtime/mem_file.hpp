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
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"
#include "lyra/value/unpacked_array.hpp"

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
// current size spans, and an associative array is addressed by key and takes a
// key prototype so it builds each key at the width an ordinary access uses. The
// element is a single packed vector in every one of them.

// What a load completes with: the memory it filled.
template <typename Memory>
using MemoryLoad = value::Tuple<Memory>;

using DynamicMemory = value::DynamicArray<value::PackedArray>;
using QueueMemory = value::Queue<value::PackedArray>;
using AssociativeMemory =
    value::AssociativeArray<value::PackedArray, value::PackedArray>;

auto ReadMem(
    RuntimeEffects& runtime, DynamicMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<DynamicMemory>;
auto ReadMemWithin(
    RuntimeEffects& runtime, DynamicMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) -> MemoryLoad<DynamicMemory>;
void WriteMem(
    RuntimeEffects& runtime, const DynamicMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMemWithin(
    RuntimeEffects& runtime, const DynamicMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

auto ReadMem(
    RuntimeEffects& runtime, QueueMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<QueueMemory>;
auto ReadMemWithin(
    RuntimeEffects& runtime, QueueMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) -> MemoryLoad<QueueMemory>;
void WriteMem(
    RuntimeEffects& runtime, const QueueMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMemWithin(
    RuntimeEffects& runtime, const QueueMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

auto ReadMem(
    RuntimeEffects& runtime, AssociativeMemory dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<AssociativeMemory>;
auto ReadMemWithin(
    RuntimeEffects& runtime, AssociativeMemory dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) -> MemoryLoad<AssociativeMemory>;
void WriteMem(
    RuntimeEffects& runtime, const AssociativeMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start);
void WriteMemWithin(
    RuntimeEffects& runtime, const AssociativeMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

// The same four memories, held as the erased values the execution backend
// realizes them by. A load answers with the memory it filled rather than with
// a completion, because what a completion is made of belongs to the boundary
// that builds one. Nothing about the addressing changes: the words are taken
// out in address order, the same cores fill or render them, and the memory is
// rebuilt around them -- which is what a value that cannot be written in place
// needs in order to keep the words the file does not reach.
auto ReadMem(
    RuntimeEffects& runtime, const value::RuntimeUnpackedArray& dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start,
    std::optional<std::int64_t> finish) -> value::RuntimeUnpackedArray;
void WriteMem(
    RuntimeEffects& runtime, const value::RuntimeUnpackedArray& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start,
    std::optional<std::int64_t> finish);

auto ReadMem(
    RuntimeEffects& runtime, const value::RuntimeDynamicArray& dest,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, std::optional<std::int64_t> finish)
    -> value::RuntimeDynamicArray;
void WriteMem(
    RuntimeEffects& runtime, const value::RuntimeDynamicArray& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, std::optional<std::int64_t> finish);

auto ReadMem(
    RuntimeEffects& runtime, const value::RuntimeQueue& dest,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, std::optional<std::int64_t> finish)
    -> value::RuntimeQueue;
void WriteMem(
    RuntimeEffects& runtime, const value::RuntimeQueue& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, std::optional<std::int64_t> finish);

auto ReadMem(
    RuntimeEffects& runtime, const value::RuntimeAssociativeArray& dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start,
    std::optional<std::int64_t> finish) -> value::RuntimeAssociativeArray;
void WriteMem(
    RuntimeEffects& runtime, const value::RuntimeAssociativeArray& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, std::optional<std::int64_t> finish);

// LRM 21.4.3: multidimensional memory support. The type-driven navigation lives
// in these header templates -- arbitrary nesting depth, traversed in ascending
// address at every dimension (the file is address-ordered even when a dimension
// is declared descending) -- while the file / parse / render / diagnostic logic
// stays in the non-template cores below, reached through a leaf accessor. The
// dimensions ride as a `[left0, right0, left1, right1, ...]` bounds array: the
// first pair is the highest (addressed) dimension, the rest describe the leaves
// each highest-dimension word expands to, row-major.
namespace detail {

// A level of a memory: an array of the level below, or the packed word the
// nesting bottoms out at. A one-dimensional memory is the depth-one case of
// the same traversal, so it needs no form of its own.
template <typename T>
struct IsMemoryLevel : std::false_type {};
template <>
struct IsMemoryLevel<value::PackedArray> : std::true_type {};
template <typename U>
struct IsMemoryLevel<value::UnpackedArray<U>> : std::true_type {};

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
  requires detail::IsMemoryLevel<Inner>::value
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
  requires detail::IsMemoryLevel<Inner>::value
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

// An unpacked memory of any depth. The bounds ride as a
// `[left0, right0, left1, right1, ...]` array whose first pair is the addressed
// dimension and whose rest describe the leaves each address expands to, so a
// one-dimensional memory is the two-element case of the same traversal.
template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
auto ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<Inner> dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<value::UnpackedArray<Inner>> {
  ReadMemMultidim(
      runtime, dest, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
  return MemoryLoad<value::UnpackedArray<Inner>>{std::move(dest)};
}

template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
auto ReadMemWithin(
    RuntimeEffects& runtime, value::UnpackedArray<Inner> dest,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish)
    -> MemoryLoad<value::UnpackedArray<Inner>> {
  ReadMemMultidim(
      runtime, dest, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
  return MemoryLoad<value::UnpackedArray<Inner>>{std::move(dest)};
}

template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
void WriteMem(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start) {
  WriteMemMultidim(
      runtime, src, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}

template <typename Inner>
  requires detail::IsMemoryLevel<Inner>::value
void WriteMemWithin(
    RuntimeEffects& runtime, const value::UnpackedArray<Inner>& src,
    const value::String& filename, std::span<const value::PackedArray> dims,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) {
  WriteMemMultidim(
      runtime, src, filename, dims, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

}  // namespace lyra::runtime
