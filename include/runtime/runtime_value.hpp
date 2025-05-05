#pragma once

#include <cassert>
#include <cstdint>
#include <string>
#include <utility>

#include <fmt/core.h>

#include "common/literal.hpp"
#include "common/type.hpp"

namespace lyra {

struct RuntimeValue {
 public:
  common::Type type;
  common::ValueStorage value;

  static auto FromLiteral(const common::Literal& literal) -> RuntimeValue {
    return RuntimeValue{.type = literal.type, .value = literal.value};
  }

  static auto DefaultValueForType(const common::Type& type) -> RuntimeValue {
    switch (type.kind) {
      case common::Type::Kind::kVoid:
        return {};
      case common::Type::Kind::kTwoState: {
        auto data = std::get<common::TwoStateData>(type.data);
        if (data.is_signed) {
          return TwoStateSigned(0, data.bit_width);
        }
        return TwoStateUnsigned(0, data.bit_width);
      }
      case common::Type::Kind::kString:
        return String("");
    }
  }

  static auto TwoStateSigned(int64_t value, size_t bit_width) -> RuntimeValue {
    return RuntimeValue{
        .type = common::Type::TwoStateSigned(bit_width),
        .value = common::ValueStorage(value)};
  }

  static auto TwoStateUnsigned(uint64_t value, size_t bit_width)
      -> RuntimeValue {
    return RuntimeValue{
        .type = common::Type::TwoStateUnsigned(bit_width),
        .value = common::ValueStorage(static_cast<int64_t>(value))};
  }

  static auto Bool(bool value) -> RuntimeValue {
    return TwoStateUnsigned(value ? 1 : 0, 1);
  }

  static auto String(std::string value) -> RuntimeValue {
    return RuntimeValue{
        .type = common::Type::String(),
        .value = common::ValueStorage(std::move(value))};
  }

  [[nodiscard]] auto AsInt64() const -> int64_t {
    return value.AsInt64();
  }

  [[nodiscard]] auto AsUInt64() const -> uint64_t {
    return static_cast<uint64_t>(value.AsInt64());
  }

  [[nodiscard]] auto AsBool() const -> bool {
    return value.AsInt64() != 0;
  }

  [[nodiscard]] auto AsString() const -> const std::string& {
    return value.AsString();
  }

  [[nodiscard]] auto IsTwoState() const -> bool {
    return type.kind == common::Type::Kind::kTwoState;
  }

  [[nodiscard]] auto IsString() const -> bool {
    return type.kind == common::Type::Kind::kString;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return fmt::format("{}", value.ToString());
  }

  [[nodiscard]] auto operator==(const RuntimeValue& rhs) const
      -> bool = default;
};

inline auto operator<<(std::ostream& os, const RuntimeValue& value)
    -> std::ostream& {
  return os << value.ToString();
}

}  // namespace lyra

template <>
struct fmt::formatter<lyra::RuntimeValue> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::RuntimeValue& value, FormatContext& ctx) {
    return fmt::format_to(ctx.out(), "{}", value.ToString());
  }
};
