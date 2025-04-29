#pragma once

#include <memory>
#include <string>

namespace lyra {

struct Value {
  Value() = default;
  Value(const Value &) = default;
  Value(Value &&) = delete;
  auto operator=(const Value &) -> Value & = default;
  auto operator=(Value &&) -> Value & = delete;
  virtual ~Value() = default;
};

struct IntValue : Value {
  int64_t value;
  explicit IntValue(int64_t v) : value(v) {
  }
};

struct StringValue : Value {
  std::string value;
  explicit StringValue(std::string v) : value(std::move(v)) {
  }
};

class RuntimeValue {
 public:
  RuntimeValue() : impl_(std::make_shared<IntValue>(0)) {
  }

  static auto FromInt(int64_t value) -> RuntimeValue;
  static auto FromString(std::string value) -> RuntimeValue;

  [[nodiscard]] auto AsInt() const -> int64_t;
  [[nodiscard]] auto AsString() const -> const std::string &;

 private:
  std::shared_ptr<Value> impl_;
  explicit RuntimeValue(std::shared_ptr<Value> ptr);
};

}  // namespace lyra
