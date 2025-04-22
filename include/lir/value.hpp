#pragma once

#include <string>
#include <variant>

namespace volans::lir {

struct Value {
  enum class Kind { kTemp, kSignal, kLiteralInt, kLiteralString };

  Kind kind;
  std::variant<std::string, int> data;

  static auto MakeTemp(const std::string& name) -> Value {
    return Value{.kind = Kind::kTemp, .data = name};
  }

  static auto MakeSignal(const std::string& name) -> Value {
    return Value{.kind = Kind::kSignal, .data = name};
  }

  static auto MakeLiteralInt(int value) -> Value {
    return Value{.kind = Kind::kLiteralInt, .data = value};
  }

  static auto MakeLiteralString(const std::string& value) -> Value {
    return Value{.kind = Kind::kLiteralString, .data = value};
  }

  auto ToString() const -> std::string {
    switch (kind) {
      case Kind::kTemp:
      case Kind::kSignal:
      case Kind::kLiteralString:
        return std::get<std::string>(data);
      case Kind::kLiteralInt:
        return std::to_string(std::get<int>(data));
    }
    return "<invalid>";
  }
};

}  // namespace volans::lir
