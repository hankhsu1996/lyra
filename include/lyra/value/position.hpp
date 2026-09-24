#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>

namespace lyra::value {

class PackedArray;

// Where a select lands, counted in the selected value's own numbering: a bit
// from the least significant end of a packed value, an element from the left of
// an unpacked one. Whatever the source wrote to name it -- a declared range,
// its direction, which part-select form -- has already been read by then, so an
// access is handed the place itself and a run of that many parts.
//
// A position reaches an access as an integral value, because one computed while
// the program runs carries whatever the index it came from held. An x or z bit
// names no position (LRM 11.5.1, 7.4.5), and neither does a magnitude past
// every value a design can declare, which is out of range for any of them.
// This is the one reading of such a value, so every family agrees on which
// positions exist.
[[nodiscard]] auto ReadPosition(const PackedArray& position)
    -> std::optional<std::int64_t>;

// LRM 7.4.5 / 7.10.1: the element a position names in a container holding
// `size` elements, or none -- the read-default / write-discard path -- where
// the position names no element there. Every unpacked family reaches an
// element through this, so none of them can disagree about which exist.
[[nodiscard]] auto ElementOrdinal(const PackedArray& position, std::size_t size)
    -> std::optional<std::size_t>;

// Past this magnitude a position lies outside every value: no packed value is
// this wide and no unpacked one holds this many elements. Bounding a position
// here is also what keeps the arithmetic that composes positions inside a
// machine word.
inline constexpr std::int64_t kPositionLimit = std::int64_t{1} << 32;

}  // namespace lyra::value
