#include "core/value.hpp"

#include <stdexcept>

namespace lyra {

auto RuntimeValue::FromInt(int value) -> RuntimeValue {
  return RuntimeValue(std::make_shared<IntValue>(value));
}

auto RuntimeValue::FromString(std::string value) -> RuntimeValue {
  return RuntimeValue(std::make_shared<StringValue>(std::move(value)));
}

auto RuntimeValue::AsInt() const -> int {
  auto *int_value = dynamic_cast<IntValue *>(impl_.get());
  if (int_value == nullptr) {
    throw std::runtime_error("Value is not an integer");
  }
  return int_value->value;
}

auto RuntimeValue::AsString() const -> const std::string & {
  auto *string_value = dynamic_cast<StringValue *>(impl_.get());
  if (string_value == nullptr) {
    throw std::runtime_error("Value is not a string");
  }
  return string_value->value;
}

RuntimeValue::RuntimeValue(std::shared_ptr<Value> ptr) : impl_(std::move(ptr)) {
}

}  // namespace lyra
