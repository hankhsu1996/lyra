#pragma once

#include <concepts>
#include <cstdint>
#include <limits>
#include <span>
#include <string>
#include <type_traits>
#include <utility>

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

constexpr auto ValidBitsMaskForWord(std::uint64_t width, std::size_t word_index)
    -> std::uint64_t {
  constexpr std::uint64_t kWordBits = 64U;
  if (static_cast<std::uint64_t>(word_index) >
      std::numeric_limits<std::uint64_t>::max() / kWordBits) {
    throw InternalError("ValidBitsMaskForWord: word index overflow");
  }
  const auto consumed = static_cast<std::uint64_t>(word_index) * kWordBits;
  if (consumed >= width) {
    throw InternalError("ValidBitsMaskForWord: word index out of range");
  }
  const auto remaining = width - consumed;
  if (remaining >= kWordBits) {
    return ~std::uint64_t{0};
  }
  return (std::uint64_t{1} << remaining) - 1U;
}

template <typename V>
concept BitViewLike = std::same_as<std::remove_cvref_t<V>, ConstBitView> ||
                      std::same_as<std::remove_cvref_t<V>, BitView>;

template <typename V>
concept LogicViewLike = std::same_as<std::remove_cvref_t<V>, ConstLogicView> ||
                        std::same_as<std::remove_cvref_t<V>, LogicView>;

inline auto ToConstView(ConstBitView v) -> ConstBitView {
  return v;
}
inline auto ToConstView(BitView v) -> ConstBitView {
  return v.AsConst();
}
inline auto ToConstView(ConstLogicView v) -> ConstLogicView {
  return v;
}
inline auto ToConstView(LogicView v) -> ConstLogicView {
  return v.AsConst();
}

struct PlaneAccess {
  template <PackedShape Shape, Signedness Signed>
  static auto AlignedMutableValueWords(Bit<Shape, Signed>& b)
      -> std::span<std::uint64_t> {
    return b.MutableValueWordsForPackedOps();
  }
  template <PackedShape Shape, Signedness Signed>
  static auto AlignedMutableValueWords(Logic<Shape, Signed>& l)
      -> std::span<std::uint64_t> {
    return l.MutableValueWordsForPackedOps();
  }
  template <PackedShape Shape, Signedness Signed>
  static auto AlignedMutableUnknownWords(Logic<Shape, Signed>& l)
      -> std::span<std::uint64_t> {
    return l.MutableUnknownWordsForPackedOps();
  }

  static auto AlignedValueWords(ConstBitView v)
      -> std::span<const std::uint64_t> {
    if (v.BitOffsetForPackedOps() != 0U) {
      throw InternalError(
          "PlaneAccess::AlignedValueWords: ConstBitView with non-zero "
          "bit_offset is not supported");
    }
    return v.WordsForPackedOps();
  }
  static auto AlignedValueWords(ConstLogicView v)
      -> std::span<const std::uint64_t> {
    if (v.BitOffsetForPackedOps() != 0U) {
      throw InternalError(
          "PlaneAccess::AlignedValueWords: ConstLogicView with non-zero "
          "bit_offset is not supported");
    }
    return v.ValueWordsForPackedOps();
  }
  static auto AlignedUnknownWords(ConstLogicView v)
      -> std::span<const std::uint64_t> {
    if (v.BitOffsetForPackedOps() != 0U) {
      throw InternalError(
          "PlaneAccess::AlignedUnknownWords: ConstLogicView with non-zero "
          "bit_offset is not supported");
    }
    return v.UnknownWordsForPackedOps();
  }
};

using ScalarShape = PackedShape<0>;

inline auto MakeScalarBit(bool value)
    -> Bit<ScalarShape{}, Signedness::kUnsigned> {
  Bit<ScalarShape{}, Signedness::kUnsigned> out;
  PlaneAccess::AlignedMutableValueWords(out)[0] =
      value ? std::uint64_t{1} : std::uint64_t{0};
  return out;
}

inline auto MakeScalarLogicKnown(bool value)
    -> Logic<ScalarShape{}, Signedness::kUnsigned> {
  Logic<ScalarShape{}, Signedness::kUnsigned> out;
  PlaneAccess::AlignedMutableValueWords(out)[0] =
      value ? std::uint64_t{1} : std::uint64_t{0};
  PlaneAccess::AlignedMutableUnknownWords(out)[0] = std::uint64_t{0};
  return out;
}

inline auto MakeScalarLogicX() -> Logic<ScalarShape{}, Signedness::kUnsigned> {
  Logic<ScalarShape{}, Signedness::kUnsigned> out;
  PlaneAccess::AlignedMutableValueWords(out)[0] = std::uint64_t{1};
  PlaneAccess::AlignedMutableUnknownWords(out)[0] = std::uint64_t{1};
  return out;
}

inline auto MakeScalarLogicZ() -> Logic<ScalarShape{}, Signedness::kUnsigned> {
  Logic<ScalarShape{}, Signedness::kUnsigned> out;
  PlaneAccess::AlignedMutableValueWords(out)[0] = std::uint64_t{0};
  PlaneAccess::AlignedMutableUnknownWords(out)[0] = std::uint64_t{1};
  return out;
}

inline auto NotScalar(Bit<ScalarShape{}, Signedness::kUnsigned> in)
    -> Bit<ScalarShape{}, Signedness::kUnsigned> {
  const auto view = std::as_const(in).View();
  const auto bit = PlaneAccess::AlignedValueWords(view)[0] & std::uint64_t{1};
  return MakeScalarBit(bit == 0U);
}

inline auto NotScalar(Logic<ScalarShape{}, Signedness::kUnsigned> in)
    -> Logic<ScalarShape{}, Signedness::kUnsigned> {
  const auto view = std::as_const(in).View();
  const auto unknown =
      PlaneAccess::AlignedUnknownWords(view)[0] & std::uint64_t{1};
  if (unknown != 0U) {
    return MakeScalarLogicX();
  }
  const auto value = PlaneAccess::AlignedValueWords(view)[0] & std::uint64_t{1};
  return MakeScalarLogicKnown(value == 0U);
}

}  // namespace lyra::runtime::detail
