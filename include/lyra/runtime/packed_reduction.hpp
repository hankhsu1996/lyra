#pragma once

#include "lyra/runtime/packed.hpp"

namespace lyra::runtime {

auto ReductionAnd(ConstBitView src, BitView dst) -> void;
auto ReductionAnd(ConstLogicView src, LogicView dst) -> void;

auto ReductionOr(ConstBitView src, BitView dst) -> void;
auto ReductionOr(ConstLogicView src, LogicView dst) -> void;

auto ReductionXor(ConstBitView src, BitView dst) -> void;
auto ReductionXor(ConstLogicView src, LogicView dst) -> void;

auto ReductionNand(ConstBitView src, BitView dst) -> void;
auto ReductionNand(ConstLogicView src, LogicView dst) -> void;

auto ReductionNor(ConstBitView src, BitView dst) -> void;
auto ReductionNor(ConstLogicView src, LogicView dst) -> void;

auto ReductionXnor(ConstBitView src, BitView dst) -> void;
auto ReductionXnor(ConstLogicView src, LogicView dst) -> void;

inline auto ReductionAnd(BitView src, BitView dst) -> void {
  ReductionAnd(src.AsConst(), dst);
}

inline auto ReductionAnd(LogicView src, LogicView dst) -> void {
  ReductionAnd(src.AsConst(), dst);
}

inline auto ReductionOr(BitView src, BitView dst) -> void {
  ReductionOr(src.AsConst(), dst);
}

inline auto ReductionOr(LogicView src, LogicView dst) -> void {
  ReductionOr(src.AsConst(), dst);
}

inline auto ReductionXor(BitView src, BitView dst) -> void {
  ReductionXor(src.AsConst(), dst);
}

inline auto ReductionXor(LogicView src, LogicView dst) -> void {
  ReductionXor(src.AsConst(), dst);
}

inline auto ReductionNand(BitView src, BitView dst) -> void {
  ReductionNand(src.AsConst(), dst);
}

inline auto ReductionNand(LogicView src, LogicView dst) -> void {
  ReductionNand(src.AsConst(), dst);
}

inline auto ReductionNor(BitView src, BitView dst) -> void {
  ReductionNor(src.AsConst(), dst);
}

inline auto ReductionNor(LogicView src, LogicView dst) -> void {
  ReductionNor(src.AsConst(), dst);
}

inline auto ReductionXnor(BitView src, BitView dst) -> void {
  ReductionXnor(src.AsConst(), dst);
}

inline auto ReductionXnor(LogicView src, LogicView dst) -> void {
  ReductionXnor(src.AsConst(), dst);
}

}  // namespace lyra::runtime
