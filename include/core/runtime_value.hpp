#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>
#include <variant>

namespace lyra {

enum class ValueKind {
  kBit,
  kInt,
  kLongInt,
  kString,
};

class RuntimeValue {
 public:
  using Storage = std::variant<bool, int32_t, int64_t, std::string>;

  RuntimeValue() : kind_(ValueKind::kInt), storage_(int32_t{0}) {
  }

  static auto FromBit(bool value) -> RuntimeValue {
    return {ValueKind::kBit, value};
  }

  static auto FromInt(int32_t value) -> RuntimeValue {
    return {ValueKind::kInt, value};
  }

  static auto FromLongInt(int64_t value) -> RuntimeValue {
    return {ValueKind::kLongInt, value};
  }

  static auto FromString(std::string value) -> RuntimeValue {
    return {ValueKind::kString, std::move(value)};
  }

  [[nodiscard]] auto Kind() const -> ValueKind {
    return kind_;
  }

  [[nodiscard]] auto AsBit() const -> bool {
    CheckKind(ValueKind::kBit);
    return std::get<bool>(storage_);
  }

  [[nodiscard]] auto AsInt() const -> int32_t {
    CheckKind(ValueKind::kInt);
    return std::get<int32_t>(storage_);
  }

  [[nodiscard]] auto AsLongInt() const -> int64_t {
    CheckKind(ValueKind::kLongInt);
    return std::get<int64_t>(storage_);
  }

  [[nodiscard]] auto AsString() const -> const std::string& {
    CheckKind(ValueKind::kString);
    return std::get<std::string>(storage_);
  }

 private:
  ValueKind kind_;
  Storage storage_;

  RuntimeValue(ValueKind kind, Storage value)
      : kind_(kind), storage_(std::move(value)) {
  }

  void CheckKind(ValueKind expected) const {
    if (kind_ != expected) {
      throw std::runtime_error("RuntimeValue kind mismatch");
    }
  }
};

}  // namespace lyra
