#include "lyra/interpreter/builtin_ops.hpp"

#include <bit>
#include <cassert>

namespace lyra::interpreter {

// Unary Operations

auto UnaryPlus(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  // Unary plus is a no-op
  return operand;
}

auto UnaryMinus(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        -operand.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      static_cast<uint64_t>(-static_cast<int64_t>(operand.AsUInt64())),
      two_state_data.bit_width);
}

auto UnaryLogicalNot(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  // Logical NOT: 0 if non-zero, 1 if zero
  return RuntimeValue::Bool(operand.AsInt64() == 0);
}

auto UnaryBitwiseNot(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        ~operand.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      ~operand.AsUInt64(), two_state_data.bit_width);
}

// Reduction Operations

auto ReductionAnd(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // AND reduction: 1 if all bits are 1, 0 otherwise
  return RuntimeValue::Bool(value == mask);
}

auto ReductionNand(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // NAND reduction: 0 if all bits are 1, 1 otherwise
  return RuntimeValue::Bool(value != mask);
}

auto ReductionOr(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // OR reduction: 1 if any bit is 1, 0 otherwise
  return RuntimeValue::Bool(value != 0);
}

auto ReductionNor(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // NOR reduction: 0 if any bit is 1, 1 otherwise
  return RuntimeValue::Bool(value == 0);
}

auto ReductionXor(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // XOR reduction: true if popcount is odd
  return RuntimeValue::Bool(std::popcount(value) % 2 == 1);
}

auto ReductionXnor(const RuntimeValue& operand) -> RuntimeValue {
  assert(operand.type.kind == common::Type::Kind::kTwoState);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);
  assert(two_state_data.bit_width <= 64);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // XNOR reduction: true if popcount is even
  return RuntimeValue::Bool(std::popcount(value) % 2 == 0);
}

// Binary Operations

auto BinaryAdd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() + rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() + rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinarySubtract(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() - rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() - rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryMultiply(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() * rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() * rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryDivide(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() / rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() / rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryModulo(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() % rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() % rhs.AsUInt64(), two_state_data.bit_width);
}

// support string.
auto BinaryEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type == rhs.type);

  if (lhs.IsTwoState()) {
    return RuntimeValue::Bool(lhs.AsInt64() == rhs.AsInt64());
  }

  if (lhs.IsString()) {
    return RuntimeValue::Bool(lhs.AsString() == rhs.AsString());
  }
  std::abort();
}

auto BinaryNotEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type == rhs.type);

  if (lhs.IsTwoState()) {
    return RuntimeValue::Bool(lhs.AsInt64() != rhs.AsInt64());
  }

  if (lhs.IsString()) {
    return RuntimeValue::Bool(lhs.AsString() != rhs.AsString());
  }
  std::abort();
}

auto BinaryLessThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() < rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() < rhs.AsUInt64());
}

auto BinaryLessThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() <= rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() <= rhs.AsUInt64());
}

auto BinaryGreaterThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() > rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() > rhs.AsUInt64());
}

auto BinaryGreaterThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() >= rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() >= rhs.AsUInt64());
}

}  // namespace lyra::interpreter
