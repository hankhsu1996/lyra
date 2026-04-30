#include "lyra/runtime/packed_convert.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/runtime/packed.hpp"

namespace lyra::runtime {

auto ConvertToBit(ConstBitView src, BitView dst, Signedness src_signedness)
    -> void {
  dst.SetZero();
  const std::uint64_t copy_w = std::min(src.Width(), dst.Width());
  for (std::uint64_t i = 0; i < copy_w; ++i) {
    dst.SetBit(i, src.GetBit(i));
  }
  if (dst.Width() > src.Width() && src_signedness == Signedness::kSigned) {
    const TwoStateBit sign = src.GetBit(src.Width() - 1U);
    if (sign == TwoStateBit::kOne) {
      for (std::uint64_t i = src.Width(); i < dst.Width(); ++i) {
        dst.SetBit(i, TwoStateBit::kOne);
      }
    }
  }
}

auto ConvertToBit(ConstLogicView src, BitView dst, Signedness src_signedness)
    -> void {
  dst.SetZero();
  const std::uint64_t copy_w = std::min(src.Width(), dst.Width());
  for (std::uint64_t i = 0; i < copy_w; ++i) {
    const FourStateBit b = src.GetBit(i);
    dst.SetBit(
        i, b == FourStateBit::kOne ? TwoStateBit::kOne : TwoStateBit::kZero);
  }
  if (dst.Width() > src.Width() && src_signedness == Signedness::kSigned) {
    const FourStateBit sign = src.GetBit(src.Width() - 1U);
    if (sign == FourStateBit::kOne) {
      for (std::uint64_t i = src.Width(); i < dst.Width(); ++i) {
        dst.SetBit(i, TwoStateBit::kOne);
      }
    }
  }
}

auto ConvertToLogic(ConstBitView src, LogicView dst, Signedness src_signedness)
    -> void {
  dst.SetZero();
  const std::uint64_t copy_w = std::min(src.Width(), dst.Width());
  for (std::uint64_t i = 0; i < copy_w; ++i) {
    const TwoStateBit b = src.GetBit(i);
    dst.SetBit(
        i, b == TwoStateBit::kOne ? FourStateBit::kOne : FourStateBit::kZero);
  }
  if (dst.Width() > src.Width() && src_signedness == Signedness::kSigned) {
    const TwoStateBit sign = src.GetBit(src.Width() - 1U);
    if (sign == TwoStateBit::kOne) {
      for (std::uint64_t i = src.Width(); i < dst.Width(); ++i) {
        dst.SetBit(i, FourStateBit::kOne);
      }
    }
  }
}

auto ConvertToLogic(
    ConstLogicView src, LogicView dst, Signedness src_signedness) -> void {
  dst.SetZero();
  const std::uint64_t copy_w = std::min(src.Width(), dst.Width());
  for (std::uint64_t i = 0; i < copy_w; ++i) {
    dst.SetBit(i, src.GetBit(i));
  }
  if (dst.Width() > src.Width() && src_signedness == Signedness::kSigned) {
    const FourStateBit sign = src.GetBit(src.Width() - 1U);
    if (sign != FourStateBit::kZero) {
      for (std::uint64_t i = src.Width(); i < dst.Width(); ++i) {
        dst.SetBit(i, sign);
      }
    }
  }
}

}  // namespace lyra::runtime
