#pragma once

#include <algorithm>
#include <functional>

#include <core/runtime_value.hpp>

namespace lyra {

// Represents promotion order for arithmetic-compatible types
enum class ValueRank {
  kBit = 0,
  kInt32 = 1,
  kInt64 = 2,
};

// Maps RuntimeValue to its promotion rank
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

// A pair of promoted values and their common rank
struct PromotedPair {
  RuntimeValue lhs;
  RuntimeValue rhs;
  ValueRank rank{};
};

// Promotes two RuntimeValues to a common type
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
    std::abort();
  };

  return PromotedPair{
      .lhs = promote_one(lhs, lhs_rank, rank),
      .rhs = promote_one(rhs, rhs_rank, rank),
      .rank = rank};
}

// Generates a binary arithmetic operation that handles promotion
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

// Arithmetic operators
inline const auto kAddOp = MakeArithmeticOp(std::plus<>{});
inline const auto kSubOp = MakeArithmeticOp(std::minus<>{});
inline const auto kMulOp = MakeArithmeticOp(std::multiplies<>{});
inline const auto kDivOp = MakeArithmeticOp(std::divides<>{});
inline const auto kModOp = MakeArithmeticOp(std::modulus<>{});

// Comparison operators
inline const auto kEqOp = MakeArithmeticOp(std::equal_to<>{});
inline const auto kNeOp = MakeArithmeticOp(std::not_equal_to<>{});
inline const auto kLtOp = MakeArithmeticOp(std::less<>{});
inline const auto kLeOp = MakeArithmeticOp(std::less_equal<>{});
inline const auto kGtOp = MakeArithmeticOp(std::greater<>{});
inline const auto kGeOp = MakeArithmeticOp(std::greater_equal<>{});

}  // namespace lyra
