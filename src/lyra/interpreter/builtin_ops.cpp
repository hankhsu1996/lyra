#include "lyra/interpreter/builtin_ops.hpp"

#include <bit>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <format>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
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

void CheckIntegral(const char* op_name, const RuntimeValue& val) {
  if (!val.IsTwoState()) {
    TypeError(op_name, val, "TwoState");
  }
}

void CheckBinaryTwoState(
    const char* op_name, const RuntimeValue& lhs, const RuntimeValue& rhs) {
  if (!lhs.IsTwoState() || !rhs.IsTwoState()) {
    TypeMismatch(op_name, lhs, rhs);
  }
  if (lhs.type != rhs.type) {
    TypeMismatch(op_name, lhs, rhs);
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

  CheckIntegral("UnaryPlus", operand);

  // Unary plus is a no-op
  return operand;
}

auto UnaryMinus(const RuntimeValue& operand) -> RuntimeValue {
  if (IsFloating(operand)) {
    return RuntimeValue::Real(-AsDoubleValue(operand));
  }

  CheckIntegral("UnaryMinus", operand);
  auto two_state_data = std::get<common::IntegralData>(operand.type.data);

  if (operand.IsWide()) {
    return RuntimeValue::IntegralWide(
        operand.AsWideBit().Negate(two_state_data.bit_width),
        two_state_data.bit_width, two_state_data.is_signed);
  }

  auto narrow = operand.AsNarrow();
  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        -narrow.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      static_cast<uint64_t>(-static_cast<int64_t>(narrow.AsUInt64())),
      two_state_data.bit_width);
}

auto UnaryLogicalNot(const RuntimeValue& operand) -> RuntimeValue {
  if (IsFloating(operand)) {
    return RuntimeValue::Bool(AsDoubleValue(operand) == 0.0);
  }

  CheckIntegral("UnaryLogicalNot", operand);

  // Logical NOT: 0 if non-zero, 1 if zero
  if (operand.IsWide()) {
    return RuntimeValue::Bool(operand.AsWideBit().IsZero());
  }
  return RuntimeValue::Bool(operand.AsNarrow().raw == 0);
}

auto UnaryBitwiseNot(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("UnaryBitwiseNot", operand);
  auto two_state_data = std::get<common::IntegralData>(operand.type.data);

  if (operand.IsWide()) {
    return RuntimeValue::IntegralWide(
        operand.AsWideBit().BitwiseNot(two_state_data.bit_width),
        two_state_data.bit_width, two_state_data.is_signed);
  }

  auto narrow = operand.AsNarrow();
  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        ~narrow.AsInt64(), two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      ~narrow.AsUInt64(), two_state_data.bit_width);
}

// Reduction Operations

auto ReductionAnd(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("ReductionAnd", operand);
  auto two_state_data = std::get<common::IntegralData>(operand.type.data);

  if (operand.IsWide()) {
    const auto& wide = operand.AsWideBit();
    size_t num_words = wide.NumWords();
    size_t final_word_idx = num_words - 1;
    uint64_t final_mask =
        common::wide_ops::FinalWordMask(two_state_data.bit_width);

    // All words except last must be all-1s
    for (size_t i = 0; i < final_word_idx; ++i) {
      if (wide.GetWord(i) != ~0ULL) {
        return RuntimeValue::Bool(false);
      }
    }
    // Final word: check significant bits
    return RuntimeValue::Bool(
        (wide.GetWord(final_word_idx) & final_mask) == final_mask);
  }

  uint64_t value = operand.AsNarrow().AsUInt64();
  uint64_t mask = common::MakeBitMask(two_state_data.bit_width);
  value &= mask;  // Apply bit width mask

  // AND reduction: 1 if all bits are 1, 0 otherwise
  return RuntimeValue::Bool(value == mask);
}

auto ReductionNand(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("ReductionNand", operand);
  auto two_state_data = std::get<common::IntegralData>(operand.type.data);

  if (operand.IsWide()) {
    const auto& wide = operand.AsWideBit();
    size_t num_words = wide.NumWords();
    size_t final_word_idx = num_words - 1;
    uint64_t final_mask =
        common::wide_ops::FinalWordMask(two_state_data.bit_width);

    // NAND: NOT of AND - true if any bit is 0
    for (size_t i = 0; i < final_word_idx; ++i) {
      if (wide.GetWord(i) != ~0ULL) {
        return RuntimeValue::Bool(true);
      }
    }
    return RuntimeValue::Bool(
        (wide.GetWord(final_word_idx) & final_mask) != final_mask);
  }

  uint64_t value = operand.AsNarrow().AsUInt64();
  uint64_t mask = common::MakeBitMask(two_state_data.bit_width);
  value &= mask;  // Apply bit width mask

  // NAND reduction: 0 if all bits are 1, 1 otherwise
  return RuntimeValue::Bool(value != mask);
}

auto ReductionOr(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("ReductionOr", operand);

  if (operand.IsWide()) {
    const auto& wide = operand.AsWideBit();
    // OR: true if any bit is set (i.e., not all zeros)
    return RuntimeValue::Bool(!common::wide_ops::IsZero(wide.Words()));
  }

  auto two_state_data = std::get<common::IntegralData>(operand.type.data);
  uint64_t value = operand.AsNarrow().AsUInt64();
  uint64_t mask = common::MakeBitMask(two_state_data.bit_width);
  value &= mask;  // Apply bit width mask

  // OR reduction: 1 if any bit is 1, 0 otherwise
  return RuntimeValue::Bool(value != 0);
}

auto ReductionNor(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("ReductionNor", operand);

  if (operand.IsWide()) {
    const auto& wide = operand.AsWideBit();
    // NOR: NOT of OR - true if all bits are zero
    return RuntimeValue::Bool(common::wide_ops::IsZero(wide.Words()));
  }

  auto two_state_data = std::get<common::IntegralData>(operand.type.data);
  uint64_t value = operand.AsNarrow().AsUInt64();
  uint64_t mask = common::MakeBitMask(two_state_data.bit_width);
  value &= mask;  // Apply bit width mask

  // NOR reduction: 0 if any bit is 1, 1 otherwise
  return RuntimeValue::Bool(value == 0);
}

auto ReductionXor(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("ReductionXor", operand);
  auto two_state_data = std::get<common::IntegralData>(operand.type.data);

  if (operand.IsWide()) {
    const auto& wide = operand.AsWideBit();
    size_t num_words = wide.NumWords();
    size_t final_word_idx = num_words - 1;
    uint64_t final_mask =
        common::wide_ops::FinalWordMask(two_state_data.bit_width);

    // XOR: count all set bits, result is 1 if count is odd
    int total_popcount = 0;
    for (size_t i = 0; i < final_word_idx; ++i) {
      total_popcount += std::popcount(wide.GetWord(i));
    }
    // Mask final word to only count significant bits
    total_popcount += std::popcount(wide.GetWord(final_word_idx) & final_mask);
    return RuntimeValue::Bool(total_popcount % 2 == 1);
  }

  uint64_t value = operand.AsNarrow().AsUInt64();
  uint64_t mask = common::MakeBitMask(two_state_data.bit_width);
  value &= mask;  // Apply bit width mask

  // XOR reduction: true if popcount is odd
  return RuntimeValue::Bool(std::popcount(value) % 2 == 1);
}

auto ReductionXnor(const RuntimeValue& operand) -> RuntimeValue {
  CheckIntegral("ReductionXnor", operand);
  auto two_state_data = std::get<common::IntegralData>(operand.type.data);

  if (operand.IsWide()) {
    const auto& wide = operand.AsWideBit();
    size_t num_words = wide.NumWords();
    size_t final_word_idx = num_words - 1;
    uint64_t final_mask =
        common::wide_ops::FinalWordMask(two_state_data.bit_width);

    // XNOR: count all set bits, result is 1 if count is even
    int total_popcount = 0;
    for (size_t i = 0; i < final_word_idx; ++i) {
      total_popcount += std::popcount(wide.GetWord(i));
    }
    // Mask final word to only count significant bits
    total_popcount += std::popcount(wide.GetWord(final_word_idx) & final_mask);
    return RuntimeValue::Bool(total_popcount % 2 == 0);
  }

  uint64_t value = operand.AsNarrow().AsUInt64();
  uint64_t mask = common::MakeBitMask(two_state_data.bit_width);
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

  CheckBinaryTwoState("BinaryAdd", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit().Add(rhs.AsWideBit(), two_state_data.bit_width),
        two_state_data.bit_width, two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        lhs.AsNarrow().AsInt64() + rhs.AsNarrow().AsInt64(),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      lhs.AsNarrow().AsUInt64() + rhs.AsNarrow().AsUInt64(),
      two_state_data.bit_width);
}

auto BinarySubtract(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinarySubtract", lhs, rhs);
    return RuntimeValue::Real(AsDoubleValue(lhs) - AsDoubleValue(rhs));
  }

  CheckBinaryTwoState("BinarySubtract", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit().Sub(rhs.AsWideBit(), two_state_data.bit_width),
        two_state_data.bit_width, two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        lhs.AsNarrow().AsInt64() - rhs.AsNarrow().AsInt64(),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      lhs.AsNarrow().AsUInt64() - rhs.AsNarrow().AsUInt64(),
      two_state_data.bit_width);
}

auto BinaryMultiply(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryMultiply", lhs, rhs);
    return RuntimeValue::Real(AsDoubleValue(lhs) * AsDoubleValue(rhs));
  }

  CheckBinaryTwoState("BinaryMultiply", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit().Mul(rhs.AsWideBit(), two_state_data.bit_width),
        two_state_data.bit_width, two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        lhs.AsNarrow().AsInt64() * rhs.AsNarrow().AsInt64(),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      lhs.AsNarrow().AsUInt64() * rhs.AsNarrow().AsUInt64(),
      two_state_data.bit_width);
}

auto BinaryDivide(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryDivide", lhs, rhs);
    // IEEE 754: division by zero returns Inf
    return RuntimeValue::Real(AsDoubleValue(lhs) / AsDoubleValue(rhs));
  }

  CheckBinaryTwoState("BinaryDivide", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    throw common::InternalError(
        "BinaryDivide", "division not supported for wide types (>64 bits)");
  }

  // Integer division by zero: return 0 (Verilator behavior)
  auto lhs_narrow = lhs.AsNarrow();
  auto rhs_narrow = rhs.AsNarrow();

  if (two_state_data.is_signed) {
    if (rhs_narrow.AsInt64() == 0) {
      return RuntimeValue::IntegralSigned(0, two_state_data.bit_width);
    }
    return RuntimeValue::IntegralSigned(
        lhs_narrow.AsInt64() / rhs_narrow.AsInt64(), two_state_data.bit_width);
  }

  if (rhs_narrow.AsUInt64() == 0) {
    return RuntimeValue::IntegralUnsigned(0, two_state_data.bit_width);
  }
  return RuntimeValue::IntegralUnsigned(
      lhs_narrow.AsUInt64() / rhs_narrow.AsUInt64(), two_state_data.bit_width);
}

auto BinaryModulo(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    throw common::InternalError(
        "BinaryModulo", "modulo not supported for floating-point values");
  }

  CheckBinaryTwoState("BinaryModulo", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    throw common::InternalError(
        "BinaryModulo", "modulo not supported for wide types (>64 bits)");
  }

  // Integer modulo by zero: return 0 (Verilator behavior)
  auto lhs_narrow = lhs.AsNarrow();
  auto rhs_narrow = rhs.AsNarrow();

  if (two_state_data.is_signed) {
    if (rhs_narrow.AsInt64() == 0) {
      return RuntimeValue::IntegralSigned(0, two_state_data.bit_width);
    }
    return RuntimeValue::IntegralSigned(
        lhs_narrow.AsInt64() % rhs_narrow.AsInt64(), two_state_data.bit_width);
  }

  if (rhs_narrow.AsUInt64() == 0) {
    return RuntimeValue::IntegralUnsigned(0, two_state_data.bit_width);
  }
  return RuntimeValue::IntegralUnsigned(
      lhs_narrow.AsUInt64() % rhs_narrow.AsUInt64(), two_state_data.bit_width);
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
    if (lhs.IsWide()) {
      return RuntimeValue::Bool(lhs.AsWideBit() == rhs.AsWideBit());
    }
    return RuntimeValue::Bool(lhs.AsNarrow().raw == rhs.AsNarrow().raw);
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
    if (lhs.IsWide()) {
      return RuntimeValue::Bool(lhs.AsWideBit() != rhs.AsWideBit());
    }
    return RuntimeValue::Bool(lhs.AsNarrow().raw != rhs.AsNarrow().raw);
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

  CheckBinaryTwoState("BinaryLessThan", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::Bool(lhs.AsWideBit().LessThan(
        rhs.AsWideBit(), two_state_data.bit_width, two_state_data.is_signed));
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(
        lhs.AsNarrow().AsInt64() < rhs.AsNarrow().AsInt64());
  }

  return RuntimeValue::Bool(
      lhs.AsNarrow().AsUInt64() < rhs.AsNarrow().AsUInt64());
}

auto BinaryLessThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLessThanEqual", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) <= AsDoubleValue(rhs));
  }

  CheckBinaryTwoState("BinaryLessThanEqual", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    // a <= b is equivalent to !(b < a)
    return RuntimeValue::Bool(!rhs.AsWideBit().LessThan(
        lhs.AsWideBit(), two_state_data.bit_width, two_state_data.is_signed));
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(
        lhs.AsNarrow().AsInt64() <= rhs.AsNarrow().AsInt64());
  }

  return RuntimeValue::Bool(
      lhs.AsNarrow().AsUInt64() <= rhs.AsNarrow().AsUInt64());
}

auto BinaryGreaterThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryGreaterThan", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) > AsDoubleValue(rhs));
  }

  CheckBinaryTwoState("BinaryGreaterThan", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    // a > b is equivalent to b < a
    return RuntimeValue::Bool(rhs.AsWideBit().LessThan(
        lhs.AsWideBit(), two_state_data.bit_width, two_state_data.is_signed));
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(
        lhs.AsNarrow().AsInt64() > rhs.AsNarrow().AsInt64());
  }

  return RuntimeValue::Bool(
      lhs.AsNarrow().AsUInt64() > rhs.AsNarrow().AsUInt64());
}

auto BinaryGreaterThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryGreaterThanEqual", lhs, rhs);
    return RuntimeValue::Bool(AsDoubleValue(lhs) >= AsDoubleValue(rhs));
  }

  CheckBinaryTwoState("BinaryGreaterThanEqual", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    // a >= b is equivalent to !(a < b)
    return RuntimeValue::Bool(!lhs.AsWideBit().LessThan(
        rhs.AsWideBit(), two_state_data.bit_width, two_state_data.is_signed));
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::Bool(
        lhs.AsNarrow().AsInt64() >= rhs.AsNarrow().AsInt64());
  }

  return RuntimeValue::Bool(
      lhs.AsNarrow().AsUInt64() >= rhs.AsNarrow().AsUInt64());
}

auto BinaryPower(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryPower", lhs, rhs);
    return RuntimeValue::Real(std::pow(AsDoubleValue(lhs), AsDoubleValue(rhs)));
  }

  CheckBinaryTwoState("BinaryPower", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide() || rhs.IsWide()) {
    throw common::InternalError(
        "BinaryPower", "power operation not supported for wide types");
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        static_cast<int64_t>(
            std::pow(lhs.AsNarrow().AsInt64(), rhs.AsNarrow().AsInt64())),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      static_cast<uint64_t>(
          std::pow(lhs.AsNarrow().AsUInt64(), rhs.AsNarrow().AsUInt64())),
      two_state_data.bit_width);
}

auto BinaryBitwiseAnd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState("BinaryBitwiseAnd", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit() & rhs.AsWideBit(), two_state_data.bit_width,
        two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        lhs.AsNarrow().AsInt64() & rhs.AsNarrow().AsInt64(),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      lhs.AsNarrow().AsUInt64() & rhs.AsNarrow().AsUInt64(),
      two_state_data.bit_width);
}

auto BinaryBitwiseOr(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState("BinaryBitwiseOr", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit() | rhs.AsWideBit(), two_state_data.bit_width,
        two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        lhs.AsNarrow().AsInt64() | rhs.AsNarrow().AsInt64(),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      lhs.AsNarrow().AsUInt64() | rhs.AsNarrow().AsUInt64(),
      two_state_data.bit_width);
}

auto BinaryBitwiseXor(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState("BinaryBitwiseXor", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit() ^ rhs.AsWideBit(), two_state_data.bit_width,
        two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        lhs.AsNarrow().AsInt64() ^ rhs.AsNarrow().AsInt64(),
        two_state_data.bit_width);
  }

  return RuntimeValue::IntegralUnsigned(
      lhs.AsNarrow().AsUInt64() ^ rhs.AsNarrow().AsUInt64(),
      two_state_data.bit_width);
}

auto BinaryBitwiseXnor(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  CheckBinaryTwoState("BinaryBitwiseXnor", lhs, rhs);
  auto two_state_data = std::get<common::IntegralData>(lhs.type.data);

  if (lhs.IsWide()) {
    // XNOR is ~(a ^ b)
    return RuntimeValue::IntegralWide(
        (lhs.AsWideBit() ^ rhs.AsWideBit())
            .BitwiseNot(two_state_data.bit_width),
        two_state_data.bit_width, two_state_data.is_signed);
  }

  if (two_state_data.is_signed) {
    // XNOR is ~(a ^ b)
    return RuntimeValue::IntegralSigned(
        ~(lhs.AsNarrow().AsInt64() ^ rhs.AsNarrow().AsInt64()),
        two_state_data.bit_width);
  }

  // XNOR is ~(a ^ b)
  return RuntimeValue::IntegralUnsigned(
      ~(lhs.AsNarrow().AsUInt64() ^ rhs.AsNarrow().AsUInt64()),
      two_state_data.bit_width);
}

auto BinaryLogicalAnd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLogicalAnd", lhs, rhs);
    return RuntimeValue::Bool(
        (AsDoubleValue(lhs) != 0.0) && (AsDoubleValue(rhs) != 0.0));
  }

  // Logical operations allow different bit widths
  CheckIntegral("BinaryLogicalAnd", lhs);
  CheckIntegral("BinaryLogicalAnd", rhs);

  // Check if non-zero (truthiness)
  auto is_truthy = [](const RuntimeValue& v) {
    if (v.IsWide()) {
      return !v.AsWideBit().IsZero();
    }
    return v.AsNarrow().raw != 0;
  };

  // Perform logical AND and return 1-bit result
  return RuntimeValue::Bool(is_truthy(lhs) && is_truthy(rhs));
}

auto BinaryLogicalOr(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (IsFloating(lhs) || IsFloating(rhs)) {
    CheckBinaryFloating("BinaryLogicalOr", lhs, rhs);
    return RuntimeValue::Bool(
        (AsDoubleValue(lhs) != 0.0) || (AsDoubleValue(rhs) != 0.0));
  }

  // Logical operations allow different bit widths
  CheckIntegral("BinaryLogicalOr", lhs);
  CheckIntegral("BinaryLogicalOr", rhs);

  // Check if non-zero (truthiness)
  auto is_truthy = [](const RuntimeValue& v) {
    if (v.IsWide()) {
      return !v.AsWideBit().IsZero();
    }
    return v.AsNarrow().raw != 0;
  };

  // Perform logical OR and return 1-bit result
  return RuntimeValue::Bool(is_truthy(lhs) || is_truthy(rhs));
}

auto BinaryLogicalShiftLeft(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Shift operations allow different types for lhs and rhs
  CheckIntegral("BinaryLogicalShiftLeft", lhs);
  CheckIntegral("BinaryLogicalShiftLeft", rhs);

  auto lhs_two_state_data = std::get<common::IntegralData>(lhs.type.data);

  // Right operand (shift amount) is always treated as unsigned
  // For wide values, only the lower 64 bits are relevant (shift amounts don't
  // need >64 bits)
  uint64_t shift_amount =
      rhs.IsWide() ? rhs.AsWideBit().GetWord(0) : rhs.AsNarrow().AsUInt64();
  auto bit_width = lhs_two_state_data.bit_width;

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit().ShiftLeft(shift_amount, bit_width), bit_width,
        lhs_two_state_data.is_signed);
  }

  // SV/Verilator: shift >= bit_width results in all zeros
  if (shift_amount >= bit_width) {
    if (lhs_two_state_data.is_signed) {
      return RuntimeValue::IntegralSigned(0, bit_width);
    }
    return RuntimeValue::IntegralUnsigned(0, bit_width);
  }

  // Create bit mask based on bit width
  uint64_t mask = common::MakeBitMask(bit_width);

  if (lhs_two_state_data.is_signed) {
    int64_t value = lhs.AsNarrow().AsInt64();
    // Perform shift and apply mask to maintain bit width
    // Cast to uint64_t for the mask operation, then back to int64_t
    auto result = static_cast<int64_t>(
        (static_cast<uint64_t>(value) << shift_amount) & mask);
    return RuntimeValue::IntegralSigned(result, bit_width);
  }

  uint64_t value = lhs.AsNarrow().AsUInt64();
  // Perform shift and apply mask to maintain bit width
  uint64_t result = (value << shift_amount) & mask;
  return RuntimeValue::IntegralUnsigned(result, bit_width);
}

auto BinaryLogicalShiftRight(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // Shift operations allow different types for lhs and rhs
  CheckIntegral("BinaryLogicalShiftRight", lhs);
  CheckIntegral("BinaryLogicalShiftRight", rhs);

  auto lhs_two_state_data = std::get<common::IntegralData>(lhs.type.data);

  uint64_t shift_amount =
      rhs.IsWide() ? rhs.AsWideBit().GetWord(0) : rhs.AsNarrow().AsUInt64();
  auto bit_width = lhs_two_state_data.bit_width;

  if (lhs.IsWide()) {
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit().ShiftRightLogical(shift_amount), bit_width,
        lhs_two_state_data.is_signed);
  }

  // SV/Verilator: shift >= bit_width results in all zeros
  if (shift_amount >= bit_width) {
    if (lhs_two_state_data.is_signed) {
      return RuntimeValue::IntegralSigned(0, bit_width);
    }
    return RuntimeValue::IntegralUnsigned(0, bit_width);
  }

  // Mask to ensure the result stays within bit-width
  uint64_t mask = common::MakeBitMask(bit_width);

  // Always treat the value as unsigned for logical shift
  uint64_t value = lhs.AsNarrow().AsUInt64();
  uint64_t result = (value >> shift_amount) & mask;

  if (lhs_two_state_data.is_signed) {
    return RuntimeValue::IntegralSigned(
        static_cast<int64_t>(result), bit_width);
  }

  return RuntimeValue::IntegralUnsigned(result, bit_width);
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
  CheckIntegral("BinaryArithmeticShiftRight", lhs);
  CheckIntegral("BinaryArithmeticShiftRight", rhs);

  auto lhs_two_state_data = std::get<common::IntegralData>(lhs.type.data);

  uint64_t shift_amount =
      rhs.IsWide() ? rhs.AsWideBit().GetWord(0) : rhs.AsNarrow().AsUInt64();
  auto bit_width = lhs_two_state_data.bit_width;

  if (lhs.IsWide()) {
    if (lhs_two_state_data.is_signed) {
      return RuntimeValue::IntegralWide(
          lhs.AsWideBit().ShiftRightArithmetic(shift_amount, bit_width),
          bit_width, true);
    }
    return RuntimeValue::IntegralWide(
        lhs.AsWideBit().ShiftRightLogical(shift_amount), bit_width, false);
  }

  if (lhs_two_state_data.is_signed) {
    int64_t value = lhs.AsNarrow().AsInt64();

    // SV/Verilator: shift >= bit_width results in all sign bits
    if (shift_amount >= bit_width) {
      // Sign-extend: all 0s if positive, all 1s if negative
      int64_t sign_extended = value >> 63;  // Arithmetic shift fills with sign
      return RuntimeValue::IntegralSigned(sign_extended, bit_width);
    }

    int64_t shifted = value >> shift_amount;
    // Mask off upper bits to match declared bit width
    return RuntimeValue::IntegralSigned(shifted, bit_width);
  }

  // SV/Verilator: unsigned shift >= bit_width results in all zeros
  if (shift_amount >= bit_width) {
    return RuntimeValue::IntegralUnsigned(0, bit_width);
  }

  uint64_t value = lhs.AsNarrow().AsUInt64();
  uint64_t shifted = value >> shift_amount;
  return RuntimeValue::IntegralUnsigned(shifted, bit_width);
}

}  // namespace lyra::interpreter
