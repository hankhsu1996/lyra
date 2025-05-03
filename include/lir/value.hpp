#pragma once

#include <ostream>
#include <string>
#include <variant>

#include <fmt/core.h>

namespace lyra::lir {

struct Value {
  enum class Kind { kTemp, kVariable, kLiteralInt, kLiteralString };

  Kind kind;
  std::variant<std::string, int64_t> data;

  static auto MakeTemp(const std::string& name) -> Value {
    return Value{.kind = Kind::kTemp, .data = name};
  }

  static auto MakeVariable(const std::string& name) -> Value {
    return Value{.kind = Kind::kVariable, .data = name};
  }

  static auto MakeLiteralInt(int64_t value) -> Value {
    return Value{.kind = Kind::kLiteralInt, .data = value};
  }

  static auto MakeLiteralString(const std::string& value) -> Value {
    return Value{.kind = Kind::kLiteralString, .data = value};
  }

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      case Kind::kTemp:
        return fmt::format("{}", std::get<std::string>(data));
      case Kind::kVariable:
        return std::get<std::string>(data);
      case Kind::kLiteralInt:
        return fmt::format("{}", std::get<int64_t>(data));
      case Kind::kLiteralString:
        return fmt::format("\"{}\"", std::get<std::string>(data));
    }
    return "<invalid>";
  }
};

inline auto operator<<(std::ostream& os, const Value& value) -> std::ostream& {
  return os << value.ToString();
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::Value> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Value& value, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", value.ToString());
  }
};
