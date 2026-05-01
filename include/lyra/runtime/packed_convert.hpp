#pragma once

#include "lyra/runtime/packed.hpp"

namespace lyra::runtime {

auto ConvertToBit(ConstBitView src, BitView dst, Signedness src_signedness)
    -> void;
auto ConvertToBit(ConstLogicView src, BitView dst, Signedness src_signedness)
    -> void;
auto ConvertToLogic(ConstBitView src, LogicView dst, Signedness src_signedness)
    -> void;
auto ConvertToLogic(
    ConstLogicView src, LogicView dst, Signedness src_signedness) -> void;

inline auto ConvertToBit(BitView src, BitView dst, Signedness src_signedness)
    -> void {
  ConvertToBit(src.AsConst(), dst, src_signedness);
}

inline auto ConvertToBit(LogicView src, BitView dst, Signedness src_signedness)
    -> void {
  ConvertToBit(src.AsConst(), dst, src_signedness);
}

inline auto ConvertToLogic(
    BitView src, LogicView dst, Signedness src_signedness) -> void {
  ConvertToLogic(src.AsConst(), dst, src_signedness);
}

inline auto ConvertToLogic(
    LogicView src, LogicView dst, Signedness src_signedness) -> void {
  ConvertToLogic(src.AsConst(), dst, src_signedness);
}

}  // namespace lyra::runtime
