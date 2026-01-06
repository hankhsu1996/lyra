#include "lyra/interpreter/builtin_ops.hpp"

#include <bit>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

namespace {

// Helper functions for internal error checking

[[noreturn]] void TypeMismatch(
    const char* op_name, const RuntimeValue& lhs, const RuntimeValue& rhs) {
  throw common::InternalError(
      op_name, std::format(
                   "type mismatch (lhs={}, rhs={})", lhs.type.ToString(),
                   rhs.type.ToString()));
}

[[noreturn]] void TypeError(
    const char* op_name, const RuntimeValue& val, const char* expected) {
  throw common::InternalError(
      op_name,
      std::format(
          "invalid type (got {}, expected {})", val.type.ToString(), expected));
}

void CheckTwoState64(const char* op_name, const RuntimeValue& val) {
  if (!val.IsTwoState()) {
    TypeError(op_name, val, "TwoState");
  }
  auto two_state_data = std::get<common::TwoStateData>(val.type.data);
  if (two_state_data.bit_width > 64) {
    throw common::InternalError(
        op_name,
        std::format("bit width {} exceeds max 64", two_state_data.bit_width));
  }
}

void CheckBinaryTwoState64(
    const char* op_name, const RuntimeValue& lhs, const RuntimeValue& rhs) {
  if (!lhs.IsTwoState() || !rhs.IsTwoState()) {
    TypeMismatch(op_name, lhs, rhs);
  }
  if (lhs.type != rhs.type) {
    TypeMismatch(op_name, lhs, rhs);
  }
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);
  if (two_state_data.bit_width > 64) {
    throw common::InternalError(
        op_name,
        std::format("bit width {} exceeds max 64", two_state_data.bit_width));
  }
}

// Helper to check if a value is any floating-point type (real or shortreal)
auto IsFloating(const RuntimeValue& val) -> bool {
  return val.IsReal() || val.IsShortReal();
}

// Helper to get double value from any floating type (for type coercion)
auto AsDoubleValue(const RuntimeValue& val) -> double {
  if (val.IsReal()) {
    return val.AsDouble();
  }
  if (val.IsShortReal()) {
    return static_cast<double>(val.AsFloat());
  }
  throw common::InternalError(
      "AsDoubleValue",
      std::format("expected floating type, got {}", val.type.ToString()));
}

// Helper to check that both operands are floating types (for coercion)
void CheckBinaryFloating(
    const char* op_name, const RuntimeValue& lhs, const RuntimeValue& rhs) {
  if (!IsFloating(lhs) || !IsFloating(rhs)) {
    TypeMismatch(op_name, lhs, rhs);
  }
}

}  // namespace

// Unary Operations

auto UnaryPlus(const RuntimeValue& operand) -> RuntimeValue {
  if (IsFloating(operand)) {
    // Promote to real for consistency
    return RuntimeValue::Real(AsDoubleValue(operand));
  }

  CheckTwoState64("UnaryPlus", operand);

  // Unary plus is a no-op
  return operand;
}

auto UnaryMinus(const RuntimeValue& operand) -> RuntimeValue {
  if (IsFloating(operand)) {
    return RuntimeValue::Real(-AsDoubleValue(operand));
  }

  CheckTwoState64("UnaryMinus", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        -operand.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      static_cast<uint64_t>(-static_cast<int64_t>(operand.AsUInt64())),
      two_state_data.bit_width);
}

auto UnaryLogicalNot(const RuntimeValue& operand) -> RuntimeValue {
  if (IsFloating(operand)) {
    return RuntimeValue::Bool(AsDoubleValue(operand) == 0.0);
  }

  CheckTwoState64("UnaryLogicalNot", operand);

  // Logical NOT: 0 if non-zero, 1 if zero
  return RuntimeValue::Bool(operand.AsInt64() == 0);
}

auto UnaryBitwiseNot(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("UnaryBitwiseNot", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        ~operand.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      ~operand.AsUInt64(), two_state_data.bit_width);
}

// Reduction Operations

auto ReductionAnd(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("ReductionAnd", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // AND reduction: 1 if all bits are 1, 0 otherwise
  return RuntimeValue::Bool(value == mask);
}

auto ReductionNand(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("ReductionNand", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // NAND reduction: 0 if all bits are 1, 1 otherwise
  return RuntimeValue::Bool(value != mask);
}

auto ReductionOr(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("ReductionOr", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // OR reduction: 1 if any bit is 1, 0 otherwise
  return RuntimeValue::Bool(value != 0);
}

auto ReductionNor(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("ReductionNor", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // NOR reduction: 0 if any bit is 1, 1 otherwise
  return RuntimeValue::Bool(value == 0);
}

auto ReductionXor(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("ReductionXor", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // XOR reduction: true if popcount is odd
  return RuntimeValue::Bool(std::popcount(value) % 2 == 1);
}

auto ReductionXnor(const RuntimeValue& operand) -> RuntimeValue {
  CheckTwoState64("ReductionXnor", operand);
  auto two_state_data = std::get<common::TwoStateData>(operand.type.data);

  uint64_t value = operand.AsUInt64();
  uint64_t mask = (1ULL << two_state_data.bit_width) - 1;
  value &= mask;  // Apply bit width mask

  // XNOR reduction: true if popcount is even
  return RuntimeValue::Bool(std::popcount(value) % 2 == 0);
}

// Binary Operations

auto BinaryAdd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryAdd", lhs, rhs);
    return RuntimeValue::Real(AsDoubleValue(lhs) + AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryAdd", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() + rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() + rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinarySubtract(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinarySubtract", lhs, rhs);
    return RuntimeValue::Real(AsDoubleValue(lhs) - AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinarySubtract", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() - rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() - rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryMultiply(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryMultiply", lhs, rhs);
    return RuntimeValue::Real(AsDoubleValue(lhs) * AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryMultiply", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() * rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() * rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryDivide(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryDivide", lhs, rhs);
    // IEEE 754: division by zero returns Inf
    return RuntimeValue::Real(AsDoubleValue(lhs) / AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryDivide", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  // Integer division by zero: return 0 (Verilator behavior)
  if (two_state_data.is_signed) {
    if (rhs.AsInt64() == 0) {
      return RuntimeValue::TwoStateSigned(0, two_state_data.bit_width);
    }
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() / rhs.AsInt64(), two_state_data.bit_width);
  }

  if (rhs.AsUInt64() == 0) {
    return RuntimeValue::TwoStateUnsigned(0, two_state_data.bit_width);
  }
  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() / rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryModulo(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    throw common::InternalError(
        "BinaryModulo", "modulo not supported for floating-point values");
  }

  CheckBinaryTwoState64("BinaryModulo", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  // Integer modulo by zero: return 0 (Verilator behavior)
  if (two_state_data.is_signed) {
    if (rhs.AsInt64() == 0) {
      return RuntimeValue::TwoStateSigned(0, two_state_data.bit_width);
    }
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() % rhs.AsInt64(), two_state_data.bit_width);
  }

  if (rhs.AsUInt64() == 0) {
    return RuntimeValue::TwoStateUnsigned(0, two_state_data.bit_width);
  }
  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() % rhs.AsUInt64(), two_state_data.bit_width);
}

// Supports TwoState, Real, ShortReal, and String types.
auto BinaryEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Handle floating-point comparisons with coercion
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryEqual", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) == AsDoubleValue(rhs));
  }

  if (lhs.type != rhs.type) {
    TypeMismatch("BinaryEqual", lhs, rhs);
  }

  if (lhs.IsTwoState()) {
    return RuntimeValue::Bool(lhs.AsInt64() == rhs.AsInt64());
  }

  if (lhs.IsString()) {
    return RuntimeValue::Bool(lhs.AsString() == rhs.AsString());
  }

  throw common::InternalError(
      "BinaryEqual", std::format("unsupported type {}", lhs.type.ToString()));
}

auto BinaryNotEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Handle floating-point comparisons with coercion
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryNotEqual", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) != AsDoubleValue(rhs));
  }

  if (lhs.type != rhs.type) {
    TypeMismatch("BinaryNotEqual", lhs, rhs);
  }

  if (lhs.IsTwoState()) {
    return RuntimeValue::Bool(lhs.AsInt64() != rhs.AsInt64());
  }

  if (lhs.IsString()) {
    return RuntimeValue::Bool(lhs.AsString() != rhs.AsString());
  }

  throw common::InternalError(
      "BinaryNotEqual",
      std::format("unsupported type {}", lhs.type.ToString()));
}

auto BinaryLessThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLessThan", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) < AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryLessThan", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() < rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() < rhs.AsUInt64());
}

auto BinaryLessThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLessThanEqual", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) <= AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryLessThanEqual", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() <= rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() <= rhs.AsUInt64());
}

auto BinaryGreaterThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryGreaterThan", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) > AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryGreaterThan", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() > rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() > rhs.AsUInt64());
}

auto BinaryGreaterThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryGreaterThanEqual", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) >= AsDoubleValue(rhs));
  }

  CheckBinaryTwoState64("BinaryGreaterThanEqual", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(lhs.AsInt64() >= rhs.AsInt64());
  }

  return RuntimeValue::Bool(lhs.AsUInt64() >= rhs.AsUInt64());
}

auto BinaryPower(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryPower", lhs, rhs);
    return RuntimeValue::Real(std::pow(AsDoubleValue(lhs), AsDoubleValue(rhs)));
  }

  CheckBinaryTwoState64("BinaryPower", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

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
  CheckBinaryTwoState64("BinaryBitwiseAnd", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() & rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() & rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryBitwiseOr(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState64("BinaryBitwiseOr", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() | rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() | rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryBitwiseXor(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState64("BinaryBitwiseXor", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  if (two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        lhs.AsInt64() ^ rhs.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(
      lhs.AsUInt64() ^ rhs.AsUInt64(), two_state_data.bit_width);
}

auto BinaryBitwiseXnor(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState64("BinaryBitwiseXnor", lhs, rhs);
  auto two_state_data = std::get<common::TwoStateData>(lhs.type.data);

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
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLogicalAnd", lhs, rhs);
    return RuntimeValue::Bool(
        (AsDoubleValue(lhs) != 0.0) && (AsDoubleValue(rhs) != 0.0));
  }

  // Logical operations allow different bit widths
  CheckTwoState64("BinaryLogicalAnd", lhs);
  CheckTwoState64("BinaryLogicalAnd", rhs);

  // For checking if non-zero, we can use AsUInt64() regardless of signedness
  bool lhs_bool = (lhs.AsUInt64() != 0);
  bool rhs_bool = (rhs.AsUInt64() != 0);

  // Perform logical AND and return 1-bit result
  return RuntimeValue::Bool(lhs_bool && rhs_bool);
}

auto BinaryLogicalOr(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLogicalOr", lhs, rhs);
    return RuntimeValue::Bool(
        (AsDoubleValue(lhs) != 0.0) || (AsDoubleValue(rhs) != 0.0));
  }

  // Logical operations allow different bit widths
  CheckTwoState64("BinaryLogicalOr", lhs);
  CheckTwoState64("BinaryLogicalOr", rhs);

  // For checking if non-zero, we can use AsUInt64() regardless of signedness
  bool lhs_bool = (lhs.AsUInt64() != 0);
  bool rhs_bool = (rhs.AsUInt64() != 0);

  // Perform logical OR and return 1-bit result
  return RuntimeValue::Bool(lhs_bool || rhs_bool);
}

auto BinaryLogicalShiftLeft(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Shift operations allow different types for lhs and rhs
  CheckTwoState64("BinaryLogicalShiftLeft", lhs);
  CheckTwoState64("BinaryLogicalShiftLeft", rhs);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  // Right operand (shift amount) is always treated as unsigned
  uint64_t shift_amount = rhs.AsUInt64();
  auto bit_width = lhs_two_state_data.bit_width;

  // SV/Verilator: shift >= bit_width results in all zeros
  if (shift_amount >= bit_width) {
    if (lhs_two_state_data.is_signed) {
      return RuntimeValue::TwoStateSigned(0, bit_width);
    }
    return RuntimeValue::TwoStateUnsigned(0, bit_width);
  }

  // Create bit mask based on bit width
  uint64_t mask = (1ULL << bit_width) - 1;

  if (lhs_two_state_data.is_signed) {
    int64_t value = lhs.AsInt64();
    // Perform shift and apply mask to maintain bit width
    // Cast to uint64_t for the mask operation, then back to int64_t
    auto result = static_cast<int64_t>(
        (static_cast<uint64_t>(value) << shift_amount) & mask);
    return RuntimeValue::TwoStateSigned(result, bit_width);
  }

  uint64_t value = lhs.AsUInt64();
  // Perform shift and apply mask to maintain bit width
  uint64_t result = (value << shift_amount) & mask;
  return RuntimeValue::TwoStateUnsigned(result, bit_width);
}

auto BinaryLogicalShiftRight(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Shift operations allow different types for lhs and rhs
  CheckTwoState64("BinaryLogicalShiftRight", lhs);
  CheckTwoState64("BinaryLogicalShiftRight", rhs);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  uint64_t shift_amount = rhs.AsUInt64();
  auto bit_width = lhs_two_state_data.bit_width;

  // SV/Verilator: shift >= bit_width results in all zeros
  if (shift_amount >= bit_width) {
    if (lhs_two_state_data.is_signed) {
      return RuntimeValue::TwoStateSigned(0, bit_width);
    }
    return RuntimeValue::TwoStateUnsigned(0, bit_width);
  }

  // Mask to ensure the result stays within bit-width
  uint64_t mask = (1ULL << bit_width) - 1;

  // Always treat the value as unsigned for logical shift
  uint64_t value = lhs.AsUInt64();
  uint64_t result = (value >> shift_amount) & mask;

  if (lhs_two_state_data.is_signed) {
    return RuntimeValue::TwoStateSigned(
        static_cast<int64_t>(result), bit_width);
  }

  return RuntimeValue::TwoStateUnsigned(result, bit_width);
}

auto BinaryArithmeticShiftLeft(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Arithmetic left shift is identical to logical left shift in behavior
  // Both fill with zeros on the right
  return BinaryLogicalShiftLeft(lhs, rhs);
}

auto BinaryArithmeticShiftRight(
    const RuntimeValue& lhs, const RuntimeValue& rhs) -> RuntimeValue {
  // Shift operations allow different types for lhs and rhs
  CheckTwoState64("BinaryArithmeticShiftRight", lhs);
  CheckTwoState64("BinaryArithmeticShiftRight", rhs);

  auto lhs_two_state_data = std::get<common::TwoStateData>(lhs.type.data);

  uint64_t shift_amount = rhs.AsUInt64();
  auto bit_width = lhs_two_state_data.bit_width;

  if (lhs_two_state_data.is_signed) {
    int64_t value = lhs.AsInt64();

    // SV/Verilator: shift >= bit_width results in all sign bits
    if (shift_amount >= bit_width) {
      // Sign-extend: all 0s if positive, all 1s if negative
      int64_t sign_extended = value >> 63;  // Arithmetic shift fills with sign
      return RuntimeValue::TwoStateSigned(sign_extended, bit_width);
    }

    int64_t shifted = value >> shift_amount;
    // Mask off upper bits to match declared bit width
    return RuntimeValue::TwoStateSigned(shifted, bit_width);
  }

  // SV/Verilator: unsigned shift >= bit_width results in all zeros
  if (shift_amount >= bit_width) {
    return RuntimeValue::TwoStateUnsigned(0, bit_width);
  }

  uint64_t value = lhs.AsUInt64();
  uint64_t shifted = value >> shift_amount;
  return RuntimeValue::TwoStateUnsigned(shifted, bit_width);
}

}  // namespace lyra::interpreter
