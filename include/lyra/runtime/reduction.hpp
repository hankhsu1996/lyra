#pragma once

#include <bit>
#include <cstdint>

#include "lyra/runtime/packed.hpp"
#include "lyra/runtime/packed_words.hpp"

namespace lyra::runtime {

template <detail::BitViewLike V>
auto ReductionAnd(V src) -> Bit<detail::ScalarShape{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstView(src);
  const auto words = detail::PlaneAccess::AlignedValueWords(v);
  const auto width = v.Width();
  bool all_one = true;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const auto mask = detail::ValidBitsMaskForWord(width, i);
    if ((words[i] & mask) != mask) {
      all_one = false;
      break;
    }
  }
  return detail::MakeScalarBit(all_one);
}

template <detail::BitViewLike V>
auto ReductionOr(V src) -> Bit<detail::ScalarShape{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstView(src);
  const auto words = detail::PlaneAccess::AlignedValueWords(v);
  const auto width = v.Width();
  bool any_one = false;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const auto mask = detail::ValidBitsMaskForWord(width, i);
    if ((words[i] & mask) != 0U) {
      any_one = true;
      break;
    }
  }
  return detail::MakeScalarBit(any_one);
}

template <detail::BitViewLike V>
auto ReductionXor(V src) -> Bit<detail::ScalarShape{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstView(src);
  const auto words = detail::PlaneAccess::AlignedValueWords(v);
  const auto width = v.Width();
  std::uint64_t parity_acc = 0;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const auto mask = detail::ValidBitsMaskForWord(width, i);
    parity_acc ^= static_cast<std::uint64_t>(std::popcount(words[i] & mask));
  }
  return detail::MakeScalarBit((parity_acc & std::uint64_t{1}) != 0U);
}

template <detail::BitViewLike V>
auto ReductionNand(V src) -> Bit<detail::ScalarShape{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionAnd(src));
}

template <detail::BitViewLike V>
auto ReductionNor(V src) -> Bit<detail::ScalarShape{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionOr(src));
}

template <detail::BitViewLike V>
auto ReductionXnor(V src) -> Bit<detail::ScalarShape{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionXor(src));
}

template <detail::LogicViewLike V>
auto ReductionAnd(V src)
    -> Logic<detail::ScalarShape{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstView(src);
  const auto val_w = detail::PlaneAccess::AlignedValueWords(v);
  const auto unknown_w = detail::PlaneAccess::AlignedUnknownWords(v);
  const auto width = v.Width();
  bool any_known_zero = false;
  bool any_unknown = false;
  for (std::size_t i = 0; i < val_w.size(); ++i) {
    const auto mask = detail::ValidBitsMaskForWord(width, i);
    const auto unknown = unknown_w[i] & mask;
    const auto value = val_w[i] & mask;
    const auto known_zero_bits = (~value) & (~unknown) & mask;
    if (unknown != 0U) {
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
    return detail::MakeScalarLogicX();
  }
  return detail::MakeScalarLogicKnown(true);
}

template <detail::LogicViewLike V>
auto ReductionOr(V src) -> Logic<detail::ScalarShape{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstView(src);
  const auto val_w = detail::PlaneAccess::AlignedValueWords(v);
  const auto unknown_w = detail::PlaneAccess::AlignedUnknownWords(v);
  const auto width = v.Width();
  bool any_known_one = false;
  bool any_unknown = false;
  for (std::size_t i = 0; i < val_w.size(); ++i) {
    const auto mask = detail::ValidBitsMaskForWord(width, i);
    const auto unknown = unknown_w[i] & mask;
    const auto value = val_w[i] & mask;
    const auto known_one_bits = value & (~unknown) & mask;
    if (unknown != 0U) {
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
    return detail::MakeScalarLogicX();
  }
  return detail::MakeScalarLogicKnown(false);
}

template <detail::LogicViewLike V>
auto ReductionXor(V src)
    -> Logic<detail::ScalarShape{}, Signedness::kUnsigned> {
  const auto v = detail::ToConstView(src);
  const auto val_w = detail::PlaneAccess::AlignedValueWords(v);
  const auto unknown_w = detail::PlaneAccess::AlignedUnknownWords(v);
  const auto width = v.Width();
  std::uint64_t parity_acc = 0;
  for (std::size_t i = 0; i < val_w.size(); ++i) {
    const auto mask = detail::ValidBitsMaskForWord(width, i);
    const auto unknown = unknown_w[i] & mask;
    const auto value = val_w[i] & mask;
    if (unknown != 0U) {
      return detail::MakeScalarLogicX();
    }
    const auto known_one_bits = value & (~unknown) & mask;
    parity_acc ^= static_cast<std::uint64_t>(std::popcount(known_one_bits));
  }
  return detail::MakeScalarLogicKnown((parity_acc & std::uint64_t{1}) != 0U);
}

template <detail::LogicViewLike V>
auto ReductionNand(V src)
    -> Logic<detail::ScalarShape{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionAnd(src));
}

template <detail::LogicViewLike V>
auto ReductionNor(V src)
    -> Logic<detail::ScalarShape{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionOr(src));
}

template <detail::LogicViewLike V>
auto ReductionXnor(V src)
    -> Logic<detail::ScalarShape{}, Signedness::kUnsigned> {
  return detail::NotScalar(ReductionXor(src));
}

}  // namespace lyra::runtime
