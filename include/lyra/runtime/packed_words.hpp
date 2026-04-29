#pragma once

#include <cstdint>
#include <span>
#include <string>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/packed.hpp"

namespace lyra::runtime::detail {

inline auto MaskUnusedTopBits(std::span<std::uint64_t> dst, std::uint64_t width)
    -> void {
  if (dst.empty()) {
    return;
  }
  const std::uint64_t tail = width % 64U;
  if (tail == 0U) {
    return;
  }
  const std::uint64_t mask = (std::uint64_t{1} << tail) - 1U;
  dst.back() &= mask;
}

struct PlaneAccess {
  template <PackedShape Shape, Signedness Signed>
  static auto MutableValue(Bit<Shape, Signed>& b) -> std::span<std::uint64_t> {
    return b.MutableValueWordsForPackedOps();
  }
  template <PackedShape Shape, Signedness Signed>
  static auto MutableValue(Logic<Shape, Signed>& l)
      -> std::span<std::uint64_t> {
    return l.MutableValueWordsForPackedOps();
  }
  template <PackedShape Shape, Signedness Signed>
  static auto MutableState(Logic<Shape, Signed>& l)
      -> std::span<std::uint64_t> {
    return l.MutableStateWordsForPackedOps();
  }

  static auto ValueWords(ConstBitView v) -> std::span<const std::uint64_t> {
    if (v.BitOffsetForPackedOps() != 0U) {
      throw InternalError(
          "PlaneAccess::ValueWords: ConstBitView with non-zero bit_offset is "
          "not supported");
    }
    return v.WordsForPackedOps();
  }
  static auto ValueWords(ConstLogicView v) -> std::span<const std::uint64_t> {
    if (v.BitOffsetForPackedOps() != 0U) {
      throw InternalError(
          "PlaneAccess::ValueWords: ConstLogicView with non-zero bit_offset "
          "is not supported");
    }
    return v.ValueWordsForPackedOps();
  }
  static auto StateWords(ConstLogicView v) -> std::span<const std::uint64_t> {
    if (v.BitOffsetForPackedOps() != 0U) {
      throw InternalError(
          "PlaneAccess::StateWords: ConstLogicView with non-zero bit_offset "
          "is not supported");
    }
    return v.StateWordsForPackedOps();
  }
};

}  // namespace lyra::runtime::detail
