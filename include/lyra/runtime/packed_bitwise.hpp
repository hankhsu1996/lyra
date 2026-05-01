#pragma once

#include "lyra/runtime/packed.hpp"

namespace lyra::runtime {

auto BitwiseNot(ConstBitView src, BitView dst) -> void;
auto BitwiseNot(ConstLogicView src, LogicView dst) -> void;

auto BitwiseAnd(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseAnd(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

auto BitwiseOr(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseOr(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

auto BitwiseXor(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseXor(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

auto BitwiseXnor(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseXnor(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

inline auto BitwiseNot(BitView src, BitView dst) -> void {
  BitwiseNot(src.AsConst(), dst);
}

inline auto BitwiseNot(LogicView src, LogicView dst) -> void {
  BitwiseNot(src.AsConst(), dst);
}

inline auto BitwiseAnd(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseAnd(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseAnd(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseAnd(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseOr(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseOr(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseOr(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseOr(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXor(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseXor(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXor(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseXor(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXnor(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseXnor(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXnor(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseXnor(lhs.AsConst(), rhs.AsConst(), dst);
}

}  // namespace lyra::runtime
