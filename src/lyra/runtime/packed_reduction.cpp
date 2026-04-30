#include "lyra/runtime/packed_reduction.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/packed.hpp"
#include "lyra/runtime/packed_internal.hpp"

namespace lyra::runtime {

namespace {

auto RequireReductionShape(
    std::string_view where, std::uint64_t src_width, std::uint64_t dst_width)
    -> void {
  if (dst_width != 1U) {
    throw InternalError(
        std::format("{}: dst_width must be 1 (got {})", where, dst_width));
  }
  if (src_width == 0U) {
    throw InternalError(std::format("{}: src_width must be >= 1", where));
  }
}

auto ValidBitsMaskForWord(std::uint64_t bit_width, std::size_t word_index)
    -> std::uint64_t {
  const std::size_t last = WordCountForBits(bit_width) - 1U;
  if (word_index < last) {
    return ~std::uint64_t{0};
  }
  const std::uint64_t used =
      bit_width - (static_cast<std::uint64_t>(last) * 64U);
  if (used == 64U) {
    return ~std::uint64_t{0};
  }
  return (std::uint64_t{1} << used) - 1U;
}

auto WriteScalar(BitView dst, TwoStateBit value) -> void {
  constexpr std::string_view kWhere = "WriteScalar(Bit)";
  if (dst.Width() != 1U) {
    throw InternalError(
        std::format("{}: dst_width must be 1 (got {})", kWhere, dst.Width()));
  }
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(dst));
  const auto words = detail::PackedAccess::ValueWords(dst);
  detail::RequireWordCount(kWhere, words, dst.Width());
  words[0] = (value == TwoStateBit::kOne) ? std::uint64_t{1} : std::uint64_t{0};
}

auto WriteScalar(LogicView dst, FourStateBit value) -> void {
  constexpr std::string_view kWhere = "WriteScalar(Logic)";
  if (dst.Width() != 1U) {
    throw InternalError(
        std::format("{}: dst_width must be 1 (got {})", kWhere, dst.Width()));
  }
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(dst));
  const auto vw = detail::PackedAccess::ValueWords(dst);
  const auto uw = detail::PackedAccess::UnknownWords(dst);
  detail::RequireWordCount(kWhere, vw, dst.Width());
  detail::RequireWordCount(kWhere, uw, dst.Width());
  switch (value) {
    case FourStateBit::kZero:
      vw[0] = 0;
      uw[0] = 0;
      return;
    case FourStateBit::kOne:
      vw[0] = 1;
      uw[0] = 0;
      return;
    case FourStateBit::kHighImpedance:
      vw[0] = 0;
      uw[0] = 1;
      return;
    case FourStateBit::kUnknown:
      vw[0] = 1;
      uw[0] = 1;
      return;
  }
  throw InternalError(std::format("{}: unknown FourStateBit value", kWhere));
}

auto NotScalar(TwoStateBit v) -> TwoStateBit {
  return (v == TwoStateBit::kZero) ? TwoStateBit::kOne : TwoStateBit::kZero;
}

auto NotScalar(FourStateBit v) -> FourStateBit {
  switch (v) {
    case FourStateBit::kZero:
      return FourStateBit::kOne;
    case FourStateBit::kOne:
      return FourStateBit::kZero;
    case FourStateBit::kUnknown:
      return FourStateBit::kUnknown;
    case FourStateBit::kHighImpedance:
      return FourStateBit::kUnknown;
  }
  throw InternalError("NotScalar(Logic): unknown FourStateBit value");
}

auto ReductionAndBitValue(ConstBitView src) -> TwoStateBit {
  constexpr std::string_view kWhere = "ReductionAnd(Bit)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  const auto words = detail::PackedAccess::ValueWords(src);
  detail::RequireWordCount(kWhere, words, src.Width());
  for (std::size_t i = 0; i < words.size(); ++i) {
    const std::uint64_t mask = ValidBitsMaskForWord(src.Width(), i);
    if ((words[i] & mask) != mask) {
      return TwoStateBit::kZero;
    }
  }
  return TwoStateBit::kOne;
}

auto ReductionOrBitValue(ConstBitView src) -> TwoStateBit {
  constexpr std::string_view kWhere = "ReductionOr(Bit)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  const auto words = detail::PackedAccess::ValueWords(src);
  detail::RequireWordCount(kWhere, words, src.Width());
  for (std::size_t i = 0; i < words.size(); ++i) {
    const std::uint64_t mask = ValidBitsMaskForWord(src.Width(), i);
    if ((words[i] & mask) != 0U) {
      return TwoStateBit::kOne;
    }
  }
  return TwoStateBit::kZero;
}

auto ReductionXorBitValue(ConstBitView src) -> TwoStateBit {
  constexpr std::string_view kWhere = "ReductionXor(Bit)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  const auto words = detail::PackedAccess::ValueWords(src);
  detail::RequireWordCount(kWhere, words, src.Width());
  int parity = 0;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const std::uint64_t mask = ValidBitsMaskForWord(src.Width(), i);
    parity ^= std::popcount(words[i] & mask) & 1;
  }
  return parity != 0 ? TwoStateBit::kOne : TwoStateBit::kZero;
}

auto ReductionAndLogicValue(ConstLogicView src) -> FourStateBit {
  constexpr std::string_view kWhere = "ReductionAnd(Logic)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  const auto vw = detail::PackedAccess::ValueWords(src);
  const auto uw = detail::PackedAccess::UnknownWords(src);
  detail::RequireWordCount(kWhere, vw, src.Width());
  detail::RequireWordCount(kWhere, uw, src.Width());
  bool saw_unknown = false;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t mask = ValidBitsMaskForWord(src.Width(), i);
    const std::uint64_t known_zero = (~uw[i]) & (~vw[i]) & mask;
    if (known_zero != 0U) {
      return FourStateBit::kZero;
    }
    if ((uw[i] & mask) != 0U) {
      saw_unknown = true;
    }
  }
  return saw_unknown ? FourStateBit::kUnknown : FourStateBit::kOne;
}

auto ReductionOrLogicValue(ConstLogicView src) -> FourStateBit {
  constexpr std::string_view kWhere = "ReductionOr(Logic)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  const auto vw = detail::PackedAccess::ValueWords(src);
  const auto uw = detail::PackedAccess::UnknownWords(src);
  detail::RequireWordCount(kWhere, vw, src.Width());
  detail::RequireWordCount(kWhere, uw, src.Width());
  bool saw_unknown = false;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t mask = ValidBitsMaskForWord(src.Width(), i);
    const std::uint64_t known_one = (~uw[i]) & vw[i] & mask;
    if (known_one != 0U) {
      return FourStateBit::kOne;
    }
    if ((uw[i] & mask) != 0U) {
      saw_unknown = true;
    }
  }
  return saw_unknown ? FourStateBit::kUnknown : FourStateBit::kZero;
}

auto ReductionXorLogicValue(ConstLogicView src) -> FourStateBit {
  constexpr std::string_view kWhere = "ReductionXor(Logic)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  const auto vw = detail::PackedAccess::ValueWords(src);
  const auto uw = detail::PackedAccess::UnknownWords(src);
  detail::RequireWordCount(kWhere, vw, src.Width());
  detail::RequireWordCount(kWhere, uw, src.Width());
  int parity = 0;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t mask = ValidBitsMaskForWord(src.Width(), i);
    if ((uw[i] & mask) != 0U) {
      return FourStateBit::kUnknown;
    }
    parity ^= std::popcount(vw[i] & mask) & 1;
  }
  return parity != 0 ? FourStateBit::kOne : FourStateBit::kZero;
}

}  // namespace

auto ReductionAnd(ConstBitView src, BitView dst) -> void {
  RequireReductionShape("ReductionAnd(Bit)", src.Width(), dst.Width());
  WriteScalar(dst, ReductionAndBitValue(src));
}

auto ReductionAnd(ConstLogicView src, LogicView dst) -> void {
  RequireReductionShape("ReductionAnd(Logic)", src.Width(), dst.Width());
  WriteScalar(dst, ReductionAndLogicValue(src));
}

auto ReductionOr(ConstBitView src, BitView dst) -> void {
  RequireReductionShape("ReductionOr(Bit)", src.Width(), dst.Width());
  WriteScalar(dst, ReductionOrBitValue(src));
}

auto ReductionOr(ConstLogicView src, LogicView dst) -> void {
  RequireReductionShape("ReductionOr(Logic)", src.Width(), dst.Width());
  WriteScalar(dst, ReductionOrLogicValue(src));
}

auto ReductionXor(ConstBitView src, BitView dst) -> void {
  RequireReductionShape("ReductionXor(Bit)", src.Width(), dst.Width());
  WriteScalar(dst, ReductionXorBitValue(src));
}

auto ReductionXor(ConstLogicView src, LogicView dst) -> void {
  RequireReductionShape("ReductionXor(Logic)", src.Width(), dst.Width());
  WriteScalar(dst, ReductionXorLogicValue(src));
}

auto ReductionNand(ConstBitView src, BitView dst) -> void {
  RequireReductionShape("ReductionNand(Bit)", src.Width(), dst.Width());
  WriteScalar(dst, NotScalar(ReductionAndBitValue(src)));
}

auto ReductionNand(ConstLogicView src, LogicView dst) -> void {
  RequireReductionShape("ReductionNand(Logic)", src.Width(), dst.Width());
  WriteScalar(dst, NotScalar(ReductionAndLogicValue(src)));
}

auto ReductionNor(ConstBitView src, BitView dst) -> void {
  RequireReductionShape("ReductionNor(Bit)", src.Width(), dst.Width());
  WriteScalar(dst, NotScalar(ReductionOrBitValue(src)));
}

auto ReductionNor(ConstLogicView src, LogicView dst) -> void {
  RequireReductionShape("ReductionNor(Logic)", src.Width(), dst.Width());
  WriteScalar(dst, NotScalar(ReductionOrLogicValue(src)));
}

auto ReductionXnor(ConstBitView src, BitView dst) -> void {
  RequireReductionShape("ReductionXnor(Bit)", src.Width(), dst.Width());
  WriteScalar(dst, NotScalar(ReductionXorBitValue(src)));
}

auto ReductionXnor(ConstLogicView src, LogicView dst) -> void {
  RequireReductionShape("ReductionXnor(Logic)", src.Width(), dst.Width());
  WriteScalar(dst, NotScalar(ReductionXorLogicValue(src)));
}

}  // namespace lyra::runtime
