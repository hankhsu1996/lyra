#pragma once

#include <bit>
#include <concepts>
#include <cstdint>
#include <limits>
#include <type_traits>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/packed.hpp"
#include "lyra/runtime/packed_words.hpp"

namespace lyra::runtime {

namespace detail {

template <typename V>
concept ReductionBitViewLike =
    std::same_as<std::remove_cvref_t<V>, ConstBitView> ||
    std::same_as<std::remove_cvref_t<V>, BitView>;

template <typename V>
concept ReductionLogicViewLike =
    std::same_as<std::remove_cvref_t<V>, ConstLogicView> ||
    std::same_as<std::remove_cvref_t<V>, LogicView>;

inline auto ToConstBitView(ConstBitView v) -> ConstBitView {
  return v;
}
inline auto ToConstBitView(BitView v) -> ConstBitView {
  return v.AsConst();
}
inline auto ToConstLogicView(ConstLogicView v) -> ConstLogicView {
  return v;
}
inline auto ToConstLogicView(LogicView v) -> ConstLogicView {
  return v.AsConst();
}

constexpr auto ValidMaskForWord(std::size_t word_index, std::uint64_t width)
    -> std::uint64_t {
  constexpr std::uint64_t kWordBits = 64U;
  if (static_cast<std::uint64_t>(word_index) >
      std::numeric_limits<std::uint64_t>::max() / kWordBits) {
    throw InternalError("packed reduction: word index overflow");
  }
  const auto consumed = static_cast<std::uint64_t>(word_index) * kWordBits;
  if (consumed >= width) {
    throw InternalError("packed reduction: word index out of range");
  }
  const auto remaining = width - consumed;
  if (remaining >= kWordBits) {
    return ~std::uint64_t{0};
  }
  return (std::uint64_t{1} << remaining) - 1U;
}

inline auto MakeScalarBit(bool value)
    -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  Bit<PackedShape<0>{}, Signedness::kUnsigned> out;
  PlaneAccess::MutableValue(out)[0] =
      value ? std::uint64_t{1} : std::uint64_t{0};
  return out;
}

inline auto MakeScalarLogicKnown(bool value)
    -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  Logic<PackedShape<0>{}, Signedness::kUnsigned> out;
  PlaneAccess::MutableValue(out)[0] =
      value ? std::uint64_t{1} : std::uint64_t{0};
  PlaneAccess::MutableState(out)[0] = std::uint64_t{0};
  return out;
}

inline auto MakeScalarLogicUnknown()
    -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  Logic<PackedShape<0>{}, Signedness::kUnsigned> out;
  PlaneAccess::MutableValue(out)[0] = std::uint64_t{1};
  PlaneAccess::MutableState(out)[0] = std::uint64_t{1};
  return out;
}

inline auto NotScalar(Bit<PackedShape<0>{}, Signedness::kUnsigned> in)
    -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto view = std::as_const(in).View();
  const auto bit = PlaneAccess::ValueWords(view)[0] & std::uint64_t{1};
  return MakeScalarBit(bit == 0U);
}

inline auto NotScalar(Logic<PackedShape<0>{}, Signedness::kUnsigned> in)
    -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto view = std::as_const(in).View();
  const auto state = PlaneAccess::StateWords(view)[0] & std::uint64_t{1};
  if (state != 0U) {
    return MakeScalarLogicUnknown();
  }
  const auto value = PlaneAccess::ValueWords(view)[0] & std::uint64_t{1};
  return MakeScalarLogicKnown(value == 0U);
}

}  // namespace detail

template <detail::ReductionBitViewLike V>
auto ReductionAnd(V src) -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstBitView(src);
  const auto words = detail::PlaneAccess::ValueWords(v);
  const auto width = v.Width();
  bool all_one = true;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const auto mask = detail::ValidMaskForWord(i, width);
    if ((words[i] & mask) != mask) {
      all_one = false;
      break;
    }
  }
  return detail::MakeScalarBit(all_one);
}

template <detail::ReductionBitViewLike V>
auto ReductionOr(V src) -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstBitView(src);
  const auto words = detail::PlaneAccess::ValueWords(v);
  const auto width = v.Width();
  bool any_one = false;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const auto mask = detail::ValidMaskForWord(i, width);
    if ((words[i] & mask) != 0U) {
      any_one = true;
      break;
    }
  }
  return detail::MakeScalarBit(any_one);
}

template <detail::ReductionBitViewLike V>
auto ReductionXor(V src) -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstBitView(src);
  const auto words = detail::PlaneAccess::ValueWords(v);
  const auto width = v.Width();
  std::uint64_t parity_acc = 0;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const auto mask = detail::ValidMaskForWord(i, width);
    parity_acc ^= static_cast<std::uint64_t>(std::popcount(words[i] & mask));
  }
  return detail::MakeScalarBit((parity_acc & std::uint64_t{1}) != 0U);
}

template <detail::ReductionBitViewLike V>
auto ReductionNand(V src) -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionAnd(src));
}

template <detail::ReductionBitViewLike V>
auto ReductionNor(V src) -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionOr(src));
}

template <detail::ReductionBitViewLike V>
auto ReductionXnor(V src) -> Bit<PackedShape<0>{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionXor(src));
}

template <detail::ReductionLogicViewLike V>
auto ReductionAnd(V src) -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstLogicView(src);
  const auto val_w = detail::PlaneAccess::ValueWords(v);
  const auto state_w = detail::PlaneAccess::StateWords(v);
  const auto width = v.Width();
  bool any_known_zero = false;
  bool any_unknown = false;
  for (std::size_t i = 0; i < val_w.size(); ++i) {
    const auto mask = detail::ValidMaskForWord(i, width);
    const auto state = state_w[i] & mask;
    const auto value = val_w[i] & mask;
    const auto known_zero_bits = (~value) & (~state) & mask;
    if (state != 0U) {
      any_unknown = true;
    }
    if (known_zero_bits != 0U) {
      any_known_zero = true;
      break;
    }
  }
  if (any_known_zero) {
    return detail::MakeScalarLogicKnown(false);
  }
  if (any_unknown) {
    return detail::MakeScalarLogicUnknown();
  }
  return detail::MakeScalarLogicKnown(true);
}

template <detail::ReductionLogicViewLike V>
auto ReductionOr(V src) -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstLogicView(src);
  const auto val_w = detail::PlaneAccess::ValueWords(v);
  const auto state_w = detail::PlaneAccess::StateWords(v);
  const auto width = v.Width();
  bool any_known_one = false;
  bool any_unknown = false;
  for (std::size_t i = 0; i < val_w.size(); ++i) {
    const auto mask = detail::ValidMaskForWord(i, width);
    const auto state = state_w[i] & mask;
    const auto value = val_w[i] & mask;
    const auto known_one_bits = value & (~state) & mask;
    if (state != 0U) {
      any_unknown = true;
    }
    if (known_one_bits != 0U) {
      any_known_one = true;
      break;
    }
  }
  if (any_known_one) {
    return detail::MakeScalarLogicKnown(true);
  }
  if (any_unknown) {
    return detail::MakeScalarLogicUnknown();
  }
  return detail::MakeScalarLogicKnown(false);
}

template <detail::ReductionLogicViewLike V>
auto ReductionXor(V src) -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstLogicView(src);
  const auto val_w = detail::PlaneAccess::ValueWords(v);
  const auto state_w = detail::PlaneAccess::StateWords(v);
  const auto width = v.Width();
  std::uint64_t parity_acc = 0;
  for (std::size_t i = 0; i < val_w.size(); ++i) {
    const auto mask = detail::ValidMaskForWord(i, width);
    const auto state = state_w[i] & mask;
    const auto value = val_w[i] & mask;
    if (state != 0U) {
      return detail::MakeScalarLogicUnknown();
    }
    const auto known_one_bits = value & (~state) & mask;
    parity_acc ^= static_cast<std::uint64_t>(std::popcount(known_one_bits));
  }
  return detail::MakeScalarLogicKnown((parity_acc & std::uint64_t{1}) != 0U);
}

template <detail::ReductionLogicViewLike V>
auto ReductionNand(V src) -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionAnd(src));
}

template <detail::ReductionLogicViewLike V>
auto ReductionNor(V src) -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionOr(src));
}

template <detail::ReductionLogicViewLike V>
auto ReductionXnor(V src) -> Logic<PackedShape<0>{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionXor(src));
}

}  // namespace lyra::runtime
