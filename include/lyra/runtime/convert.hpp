#pragma once

#include <algorithm>
#include <cstdint>
#include <span>

#include "lyra/runtime/packed.hpp"
#include "lyra/runtime/packed_words.hpp"

namespace lyra::runtime {

namespace detail {

inline auto ZeroBits(std::span<std::uint64_t> dst) -> void {
  for (auto& w : dst) {
    w = 0U;
  }
}

inline auto CopyLowBits(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> src,
    std::uint64_t bit_count) -> void {
  if (bit_count == 0U) {
    return;
  }
  const std::uint64_t full_words = bit_count / 64U;
  const std::uint64_t tail = bit_count % 64U;
  for (std::uint64_t i = 0; i < full_words; ++i) {
    dst[i] = src[i];
  }
  if (tail != 0U) {
    const std::uint64_t mask = (std::uint64_t{1} << tail) - 1U;
    dst[full_words] = (dst[full_words] & ~mask) | (src[full_words] & mask);
  }
}

inline auto AndNotLowBits(
    std::span<std::uint64_t> dst, std::span<const std::uint64_t> src,
    std::uint64_t bit_count) -> void {
  if (bit_count == 0U) {
    return;
  }
  const std::uint64_t full_words = bit_count / 64U;
  const std::uint64_t tail = bit_count % 64U;
  for (std::uint64_t i = 0; i < full_words; ++i) {
    dst[i] &= ~src[i];
  }
  if (tail != 0U) {
    const std::uint64_t mask = (std::uint64_t{1} << tail) - 1U;
    dst[full_words] = (dst[full_words] & ~mask) |
                      ((dst[full_words] & ~src[full_words]) & mask);
  }
}

inline auto FillBitRange(
    std::span<std::uint64_t> dst, std::uint64_t begin, std::uint64_t end,
    bool bit_value) -> void {
  if (begin >= end) {
    return;
  }
  const std::uint64_t fill_word = bit_value ? ~std::uint64_t{0} : 0U;

  const std::uint64_t first_word = begin / 64U;
  const std::uint64_t last_inclusive = (end - 1U) / 64U;
  const std::uint64_t first_offset = begin % 64U;
  const std::uint64_t last_bits = end - (last_inclusive * 64U);

  if (first_word == last_inclusive) {
    const std::uint64_t low_mask = (std::uint64_t{1} << first_offset) - 1U;
    const std::uint64_t high_mask = last_bits == 64U
                                        ? ~std::uint64_t{0}
                                        : (std::uint64_t{1} << last_bits) - 1U;
    const std::uint64_t mask = high_mask & ~low_mask;
    dst[first_word] = (dst[first_word] & ~mask) | (fill_word & mask);
    return;
  }

  if (first_offset != 0U) {
    const std::uint64_t low_mask = (std::uint64_t{1} << first_offset) - 1U;
    dst[first_word] = (dst[first_word] & low_mask) | (fill_word & ~low_mask);
  } else {
    dst[first_word] = fill_word;
  }
  for (std::uint64_t w = first_word + 1U; w < last_inclusive; ++w) {
    dst[w] = fill_word;
  }
  if (last_bits == 64U) {
    dst[last_inclusive] = fill_word;
  } else {
    const std::uint64_t high_mask = (std::uint64_t{1} << last_bits) - 1U;
    dst[last_inclusive] =
        (dst[last_inclusive] & ~high_mask) | (fill_word & high_mask);
  }
}

inline auto TestBit(std::span<const std::uint64_t> words, std::uint64_t index)
    -> bool {
  return ((words[index / 64U] >> (index % 64U)) & 1U) != 0U;
}

struct ConversionSource {
  std::span<const std::uint64_t> value;
  std::span<const std::uint64_t> state;
  std::uint64_t width;
  Signedness signedness;
  bool four_state;
};

struct ConversionTarget {
  std::span<std::uint64_t> value;
  std::span<std::uint64_t> state;
  std::uint64_t width;
  bool four_state;
};

inline auto ConvertPackedBits(ConversionTarget dst, ConversionSource src)
    -> void {
  ZeroBits(dst.value);
  if (dst.four_state) {
    ZeroBits(dst.state);
  }

  const std::uint64_t copy_width = std::min(dst.width, src.width);

  if (src.four_state && !dst.four_state) {
    // Logic -> Bit flattens X/Z by clearing where state is 1.
    CopyLowBits(dst.value, src.value, copy_width);
    AndNotLowBits(dst.value, src.state, copy_width);
  } else {
    CopyLowBits(dst.value, src.value, copy_width);
    if (dst.four_state && src.four_state) {
      CopyLowBits(dst.state, src.state, copy_width);
    }
  }

  if (dst.width > src.width && src.width > 0U &&
      src.signedness == Signedness::kSigned) {
    bool sign_value = false;
    bool sign_state = false;
    if (src.four_state && !dst.four_state) {
      sign_value = !TestBit(src.state, src.width - 1U) &&
                   TestBit(src.value, src.width - 1U);
    } else {
      sign_value = TestBit(src.value, src.width - 1U);
      if (src.four_state) {
        // Logic -> Logic preserves an X or Z sign bit on widening.
        sign_state = TestBit(src.state, src.width - 1U);
      }
    }
    FillBitRange(dst.value, src.width, dst.width, sign_value);
    if (dst.four_state) {
      FillBitRange(dst.state, src.width, dst.width, sign_state);
    }
  }

  MaskUnusedTopBits(dst.value, dst.width);
  if (dst.four_state) {
    MaskUnusedTopBits(dst.state, dst.width);
  }
}

}  // namespace detail

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToBit(ConstBitView src, Signedness src_signedness)
    -> Bit<TargetShape, TargetSigned> {
  Bit<TargetShape, TargetSigned> out;
  detail::ConvertPackedBits(
      detail::ConversionTarget{
          .value = detail::PlaneAccess::MutableValue(out),
          .state = {},
          .width = TargetShape.TotalWidth(),
          .four_state = false,
      },
      detail::ConversionSource{
          .value = detail::PlaneAccess::ValueWords(src),
          .state = {},
          .width = src.Width(),
          .signedness = src_signedness,
          .four_state = false,
      });
  return out;
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToBit(ConstLogicView src, Signedness src_signedness)
    -> Bit<TargetShape, TargetSigned> {
  Bit<TargetShape, TargetSigned> out;
  detail::ConvertPackedBits(
      detail::ConversionTarget{
          .value = detail::PlaneAccess::MutableValue(out),
          .state = {},
          .width = TargetShape.TotalWidth(),
          .four_state = false,
      },
      detail::ConversionSource{
          .value = detail::PlaneAccess::ValueWords(src),
          .state = detail::PlaneAccess::StateWords(src),
          .width = src.Width(),
          .signedness = src_signedness,
          .four_state = true,
      });
  return out;
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToLogic(ConstBitView src, Signedness src_signedness)
    -> Logic<TargetShape, TargetSigned> {
  Logic<TargetShape, TargetSigned> out;
  detail::ConvertPackedBits(
      detail::ConversionTarget{
          .value = detail::PlaneAccess::MutableValue(out),
          .state = detail::PlaneAccess::MutableState(out),
          .width = TargetShape.TotalWidth(),
          .four_state = true,
      },
      detail::ConversionSource{
          .value = detail::PlaneAccess::ValueWords(src),
          .state = {},
          .width = src.Width(),
          .signedness = src_signedness,
          .four_state = false,
      });
  return out;
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToLogic(ConstLogicView src, Signedness src_signedness)
    -> Logic<TargetShape, TargetSigned> {
  Logic<TargetShape, TargetSigned> out;
  detail::ConvertPackedBits(
      detail::ConversionTarget{
          .value = detail::PlaneAccess::MutableValue(out),
          .state = detail::PlaneAccess::MutableState(out),
          .width = TargetShape.TotalWidth(),
          .four_state = true,
      },
      detail::ConversionSource{
          .value = detail::PlaneAccess::ValueWords(src),
          .state = detail::PlaneAccess::StateWords(src),
          .width = src.Width(),
          .signedness = src_signedness,
          .four_state = true,
      });
  return out;
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToBit(BitView src, Signedness src_signedness)
    -> Bit<TargetShape, TargetSigned> {
  return ConvertToBit<TargetShape, TargetSigned>(src.AsConst(), src_signedness);
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToBit(LogicView src, Signedness src_signedness)
    -> Bit<TargetShape, TargetSigned> {
  return ConvertToBit<TargetShape, TargetSigned>(src.AsConst(), src_signedness);
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToLogic(BitView src, Signedness src_signedness)
    -> Logic<TargetShape, TargetSigned> {
  return ConvertToLogic<TargetShape, TargetSigned>(
      src.AsConst(), src_signedness);
}

template <PackedShape TargetShape, Signedness TargetSigned>
auto ConvertToLogic(LogicView src, Signedness src_signedness)
    -> Logic<TargetShape, TargetSigned> {
  return ConvertToLogic<TargetShape, TargetSigned>(
      src.AsConst(), src_signedness);
}

}  // namespace lyra::runtime
