#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>

namespace lyra::value {

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

// The storage a union's members overlay, each at its start (LRM 7.3).
struct OverlayLayout {
  std::uint32_t size;
  std::uint32_t align;
};

// The overlay of `members`, each stating its `size` and its `align`: as large
// as the largest, rounded up to the strictest alignment. Code laying a union
// out and code stating how large one is both read it here.
template <typename Members>
[[nodiscard]] constexpr auto OverlayLayoutOf(const Members& members)
    -> OverlayLayout {
  std::uint32_t largest = 0;
  std::uint32_t strictest = 1;
  for (const auto& member : members) {
    largest = std::max(largest, static_cast<std::uint32_t>(member.size));
    strictest = std::max(strictest, static_cast<std::uint32_t>(member.align));
  }
  return OverlayLayout{
      .size = (largest + strictest - 1) / strictest * strictest,
      .align = strictest};
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
  std::uint32_t size;
  std::uint32_t align;
};

[[nodiscard]] constexpr auto TaggedUnionLayoutOf(
    OverlayLayout overlay, std::size_t members) -> TaggedUnionLayout {
  const std::uint32_t tag = TagBytesFor(members);
  const std::uint32_t align = overlay.align > tag ? overlay.align : tag;
  const std::uint32_t tag_offset = (overlay.size + tag - 1) / tag * tag;
  return TaggedUnionLayout{
      .tag = {.offset = tag_offset, .bytes = tag},
      .size = (tag_offset + tag + align - 1) / align * align,
      .align = align};
}

}  // namespace lyra::value
