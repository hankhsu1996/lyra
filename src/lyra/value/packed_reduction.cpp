#include "lyra/value/packed_reduction.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed.hpp"

namespace lyra::value {

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

auto WriteScalar(BitView dst, TwoStateBit value) -> void {
  dst.ValueWords()[0] =
      (value == TwoStateBit::kOne) ? std::uint64_t{1} : std::uint64_t{0};
}

auto WriteScalar(LogicView dst, FourStateBit value) -> void {
  const auto vw = dst.ValueWords();
  const auto uw = dst.UnknownWords();
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
  throw InternalError("WriteScalar(Logic): unknown FourStateBit value");
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
  const auto words = src.ValueWords();
  for (std::size_t i = 0; i < words.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, src.Width());
    if ((words[i] & mask) != mask) {
      return TwoStateBit::kZero;
    }
  }
  return TwoStateBit::kOne;
}

auto ReductionOrBitValue(ConstBitView src) -> TwoStateBit {
  const auto words = src.ValueWords();
  for (std::size_t i = 0; i < words.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, src.Width());
    if ((words[i] & mask) != 0U) {
      return TwoStateBit::kOne;
    }
  }
  return TwoStateBit::kZero;
}

auto ReductionXorBitValue(ConstBitView src) -> TwoStateBit {
  const auto words = src.ValueWords();
  int parity = 0;
  for (std::size_t i = 0; i < words.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, src.Width());
    parity ^= std::popcount(words[i] & mask) & 1;
  }
  return parity != 0 ? TwoStateBit::kOne : TwoStateBit::kZero;
}

auto ReductionAndLogicValue(ConstLogicView src) -> FourStateBit {
  const auto vw = src.ValueWords();
  const auto uw = src.UnknownWords();
  bool saw_unknown = false;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, src.Width());
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
  const auto vw = src.ValueWords();
  const auto uw = src.UnknownWords();
  bool saw_unknown = false;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, src.Width());
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
  const auto vw = src.ValueWords();
  const auto uw = src.UnknownWords();
  int parity = 0;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, src.Width());
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

}  // namespace lyra::value
