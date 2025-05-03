#pragma once

#include <core/runtime_value.hpp>

namespace lyra {

enum class ValueRank {
  kBit = 0,
  kInt32 = 1,
  kInt64 = 2,
};

inline auto GetRank(const RuntimeValue& value) -> ValueRank {
  switch (value.Kind()) {
    case ValueKind::kBit:
      return ValueRank::kBit;
    case ValueKind::kInt:
      return ValueRank::kInt32;
    case ValueKind::kLongInt:
      return ValueRank::kInt64;
    default:
      std::abort();
  }
}

struct PromotedPair {
  RuntimeValue lhs;
  RuntimeValue rhs;
  ValueRank rank{};
};

inline auto Promote(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> PromotedPair {
  ValueRank lhs_rank = GetRank(lhs);
  ValueRank rhs_rank = GetRank(rhs);
  ValueRank rank = std::max(lhs_rank, rhs_rank);

  auto promote_one = [&](const RuntimeValue& val, ValueRank from,
                         ValueRank to) -> RuntimeValue {
    if (from == to) {
      return val;
    }
    switch (to) {
      case ValueRank::kBit:
        return RuntimeValue::FromBit(val.AsBit());
      case ValueRank::kInt32:
        return RuntimeValue::FromInt(val.AsBit());
      case ValueRank::kInt64:
        if (from == ValueRank::kBit) {
          return RuntimeValue::FromLongInt(val.AsBit());
        }
        return RuntimeValue::FromLongInt(val.AsInt());
    }
  };

  return PromotedPair{
      .lhs = promote_one(lhs, lhs_rank, rank),
      .rhs = promote_one(rhs, rhs_rank, rank),
      .rank = rank};
}

template <typename Op>
auto MakeArithmeticOp(Op op) {
  return [=](const RuntimeValue& lhs, const RuntimeValue& rhs) -> RuntimeValue {
    auto [a, b, rank] = Promote(lhs, rhs);
    switch (rank) {
      case ValueRank::kBit:
      case ValueRank::kInt32:
        return RuntimeValue::FromInt(op(a.AsInt(), b.AsInt()));
      case ValueRank::kInt64:
        return RuntimeValue::FromLongInt(op(a.AsLongInt(), b.AsLongInt()));
    }
    std::abort();
  };
}

inline const auto kAddOp =
    MakeArithmeticOp([](auto a, auto b) { return a + b; });
inline const auto kSubOp =
    MakeArithmeticOp([](auto a, auto b) { return a - b; });
inline const auto kMulOp =
    MakeArithmeticOp([](auto a, auto b) { return a * b; });
inline const auto kDivOp =
    MakeArithmeticOp([](auto a, auto b) { return a / b; });
inline const auto kModOp =
    MakeArithmeticOp([](auto a, auto b) { return a % b; });

}  // namespace lyra
