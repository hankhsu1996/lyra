#include "lyra/interpreter/builtin_ops.hpp"

#include <bit>
#include <cassert>
#include <cmath>

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

auto BinaryPower(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        static_cast<int64_t>(std::pow(lhs.AsInt64(), rhs.AsInt64())),
        two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      static_cast<uint64_t>(std::pow(lhs.AsUInt64(), rhs.AsUInt64())),
      two_state_data.bit_width);
}

auto BinaryBitwiseAnd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() & rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() & rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryBitwiseOr(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() | rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() | rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryBitwiseXor(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() ^ rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() ^ rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryBitwiseXnor(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);
  assert(lhs.type == rhs.type);

  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(two_state_data.bit_width <= 64);

  if (two_state_data.is_signed) {
    // XNOR is ~(a ^ b)
    return RuntimeValue::TwoStateSigned(
        ~(lhs.AsInt64() ^ rhs.AsInt64()), two_state_data.bit_width);
  }

  // XNOR is ~(a ^ b)
  return RuntimeValue::TwoStateUnsigned(
      ~(lhs.AsUInt64() ^ rhs.AsUInt64()), two_state_data.bit_width);
}

auto BinaryLogicalAnd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  auto rhs_two_state_data = std::get<common::TwoStateData>(rhs.type.data);

  assert(lhs_two_state_data.bit_width <= 64);
  assert(rhs_two_state_data.bit_width <= 64);

  // For checking if non-zero, we can use AsUInt64() regardless of signedness
  bool lhs_bool = (lhs.AsUInt64() != 0);
  bool rhs_bool = (rhs.AsUInt64() != 0);

  // Perform logical AND and return 1-bit result
  return RuntimeValue::Bool(lhs_bool && rhs_bool);
}

auto BinaryLogicalOr(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  auto rhs_two_state_data = std::get<common::TwoStateData>(rhs.type.data);

  assert(lhs_two_state_data.bit_width <= 64);
  assert(rhs_two_state_data.bit_width <= 64);

  // For checking if non-zero, we can use AsUInt64() regardless of signedness
  bool lhs_bool = (lhs.AsUInt64() != 0);
  bool rhs_bool = (rhs.AsUInt64() != 0);

  // Perform logical OR and return 1-bit result
  return RuntimeValue::Bool(lhs_bool || rhs_bool);
}

auto BinaryLogicalShiftLeft(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(lhs_two_state_data.bit_width <= 64);

  // Right operand (shift amount) is always treated as unsigned
  uint64_t shift_amount = rhs.AsUInt64();

  // Create bit mask based on bit width
  uint64_t mask = (1ULL << lhs_two_state_data.bit_width) - 1;

  if (lhs_two_state_data.is_signed) {
    int64_t value = lhs.AsInt64();
    // Perform shift and apply mask to maintain bit width
    // Cast to uint64_t for the mask operation, then back to int64_t
    auto result = static_cast<int64_t>(
        (static_cast<uint64_t>(value) << shift_amount) & mask);
    return RuntimeValue::TwoStateSigned(result, lhs_two_state_data.bit_width);
  }

  uint64_t value = lhs.AsUInt64();
  // Perform shift and apply mask to maintain bit width
  uint64_t result = (value << shift_amount) & mask;
  return RuntimeValue::TwoStateUnsigned(result, lhs_two_state_data.bit_width);
}

auto BinaryLogicalShiftRight(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(lhs_two_state_data.bit_width <= 64);

  uint64_t shift_amount = rhs.AsUInt64();

  // Mask to ensure the result stays within bit-width
  uint64_t mask = (1ULL << lhs_two_state_data.bit_width) - 1;

  // Always treat the value as unsigned for logical shift
  uint64_t value = lhs.AsUInt64();
  uint64_t result = (value >> shift_amount) & mask;

  if (lhs_two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        static_cast<int64_t>(result), lhs_two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(result, lhs_two_state_data.bit_width);
}

auto BinaryArithmeticShiftLeft(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Arithmetic left shift is identical to logical left shift in behavior
  // Both fill with zeros on the right
  return BinaryLogicalShiftLeft(lhs, rhs);
}

auto BinaryArithmeticShiftRight(
    const RuntimeValue& lhs, const RuntimeValue& rhs) -> RuntimeValue {
  assert(lhs.type.kind == common::Type::Kind::kTwoState);
  assert(rhs.type.kind == common::Type::Kind::kTwoState);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  assert(lhs_two_state_data.bit_width <= 64);

  uint64_t shift_amount = rhs.AsUInt64();

  if (lhs_two_state_data.is_signed) {
    int64_t value = lhs.AsInt64();
    int64_t shifted = value >> shift_amount;
    // Mask off upper bits to match declared bit width
    return RuntimeValue::TwoStateSigned(shifted, lhs_two_state_data.bit_width);
  }

  uint64_t value = lhs.AsUInt64();
  uint64_t shifted = value >> shift_amount;
  return RuntimeValue::TwoStateUnsigned(shifted, lhs_two_state_data.bit_width);
}

}  // namespace lyra::interpreter
