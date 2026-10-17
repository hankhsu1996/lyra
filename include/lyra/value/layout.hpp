#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/support/layout.hpp"

namespace lyra::value {

// `size` rounded up to the next multiple of `align`, as LLVM's `alignTo` names
// it.
[[nodiscard]] constexpr auto AlignTo(std::size_t size, std::size_t align)
    -> std::size_t {
  return (size + align - 1) / align * align;
}

// Where each of a sequence's elements lies: how far apart two neighbours are,
// and the whole. Generated code places an element by it and the library walks
// one by it, so it is stated once for both.
struct SequenceLayout {
  std::size_t stride;
  support::Layout whole;
};

// `count` elements laid out as `element` states, one after another.
[[nodiscard]] constexpr auto SequenceLayoutOf(
    support::Layout element, std::size_t count) -> SequenceLayout {
  const std::size_t stride = AlignTo(element.size, element.align);
  return SequenceLayout{
      .stride = stride,
      .whole = {.size = stride * count, .align = element.align}};
}

// Where each of a product's parts lies, side by side, each at its own
// alignment, as a C struct lays its members out, and the whole, rounded up to
// the strictest alignment among them.
struct ProductLayout {
  std::vector<std::size_t> offsets;
  support::Layout whole;
};

// The product of `parts`, each stating its `size` and its `align`.
template <typename Parts>
[[nodiscard]] auto ProductLayoutOf(const Parts& parts) -> ProductLayout {
  ProductLayout laid{.offsets = {}, .whole = {.size = 0, .align = 1}};
  for (const auto& part : parts) {
    const std::size_t align = part.align;
    const std::size_t offset = AlignTo(laid.whole.size, align);
    laid.offsets.push_back(offset);
    laid.whole.size = offset + part.size;
    laid.whole.align = std::max<std::size_t>(laid.whole.align, align);
  }
  laid.whole.size = AlignTo(laid.whole.size, laid.whole.align);
  return laid;
}

// The storage a union's members overlay, each at its start (LRM 7.3), of
// `members` each stating its `size` and its `align`: as large as the largest,
// rounded up to the strictest alignment.
template <typename Members>
[[nodiscard]] constexpr auto OverlayLayoutOf(const Members& members)
    -> support::Layout {
  std::size_t largest = 0;
  std::size_t strictest = 1;
  for (const auto& member : members) {
    largest = std::max<std::size_t>(largest, member.size);
    strictest = std::max<std::size_t>(strictest, member.align);
  }
  return support::Layout{
      .size = AlignTo(largest, strictest), .align = strictest};
}

// How many bytes a tagged union's tag takes: the smallest unsigned integer that
// counts `members` (LRM 7.3.2 fixes no representation).
[[nodiscard]] constexpr auto TagBytesFor(std::size_t members) -> std::uint32_t {
  std::uint32_t bytes = 1;
  while (bytes < sizeof(std::uint64_t) &&
         members > (std::uint64_t{1} << (bytes * 8U))) {
    bytes *= 2;
  }
  return bytes;
}

// Where a tagged union keeps its tag, and how many bytes it takes.
struct TagPlacement {
  std::uint32_t offset;
  std::uint32_t bytes;
};

// Where a tagged union's parts lie: its members overlaid at its start, then the
// tag at its own alignment.
struct TaggedUnionLayout {
  TagPlacement tag;
  support::Layout whole;
};

[[nodiscard]] constexpr auto TaggedUnionLayoutOf(
    support::Layout overlay, std::size_t members) -> TaggedUnionLayout {
  const std::uint32_t tag = TagBytesFor(members);
  const std::size_t align = std::max<std::size_t>(overlay.align, tag);
  const auto tag_offset =
      static_cast<std::uint32_t>(AlignTo(overlay.size, tag));
  return TaggedUnionLayout{
      .tag = {.offset = tag_offset, .bytes = tag},
      .whole = {.size = AlignTo(tag_offset + tag, align), .align = align}};
}

}  // namespace lyra::value
