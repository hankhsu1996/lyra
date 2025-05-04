#include "interpreter/builtin_ops.hpp"

#include <cassert>

namespace lyra::interpreter {

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
