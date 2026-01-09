#pragma once

#include <iostream>
#include <ostream>
#include <string>
#include <utility>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/type.hpp"
#include "lyra/common/value_storage.hpp"
#include "lyra/common/wide_bit.hpp"

namespace lyra::common {

struct Literal {
  common::Type type{};
  common::ValueStorage value{};
  bool is_string_literal = false;

  static auto Void() -> Literal {
    return {common::Type::Void(), ValueStorage::Void()};
  }

  static auto Bool(bool b) -> Literal {
    return {common::Type::Bool(), ValueStorage(static_cast<int64_t>(b))};
  }

  static auto Int(int32_t v) -> Literal {
    // Direct cast to int64_t automatically sign extends correctly
    return {common::Type::Int(), ValueStorage(static_cast<int64_t>(v))};
  }

  static auto UInt(uint32_t v) -> Literal {
    return {common::Type::UInt(), ValueStorage(static_cast<int64_t>(v))};
  }

  static auto LongInt(int64_t v) -> Literal {
    return {common::Type::LongInt(), ValueStorage(v)};
  }

  static auto ULongInt(uint64_t v) -> Literal {
    return {common::Type::ULongInt(), ValueStorage(static_cast<int64_t>(v))};
  }

  static auto IntegralSigned(int64_t v, size_t width) -> Literal {
    return {common::Type::IntegralSigned(width), ValueStorage(v)};
  }

  static auto IntegralUnsigned(uint64_t v, size_t width) -> Literal {
    return {
        common::Type::IntegralUnsigned(width),
        ValueStorage(static_cast<int64_t>(v))};
  }

  // Factory for wide integral values (>64 bits)
  static auto IntegralWide(WideBit value, size_t width, bool is_signed)
      -> Literal {
    return {
        is_signed ? common::Type::IntegralSigned(width)
                  : common::Type::IntegralUnsigned(width),
        ValueStorage(std::move(value))};
  }

  static auto String(std::string v) -> Literal {
    return Literal{common::Type::String(), ValueStorage(std::move(v)), true};
  }

  static auto Real(double v) -> Literal {
    return {common::Type::Real(), ValueStorage(v)};
  }

  static auto ShortReal(float v) -> Literal {
    return {common::Type::ShortReal(), ValueStorage(v)};
  }

  // Generic factory for creating a literal from type and value.
  // Used by MIR-to-LIR when handling BitPackedStringExpression.
  static auto FromTypeAndValue(
      common::Type type, ValueStorage value, bool is_string_literal = false)
      -> Literal {
    return Literal{std::move(type), std::move(value), is_string_literal};
  }

  auto operator==(const Literal& other) const -> bool = default;

  [[nodiscard]] auto ToString() const -> std::string {
    return value.ToString();
  }

  [[nodiscard]] auto Hash() const -> std::size_t {
    std::size_t h = 0;

    std::size_t type_hash = type.Hash();
    h ^= type_hash + 0x9e3779b9 + (h << 6) + (h >> 2);

    std::size_t value_hash = value.Hash();
    h ^= value_hash + 0x9e3779b9 + (h << 6) + (h >> 2);

    return h;
  }

 private:
  Literal(common::Type t, ValueStorage v, bool is_str = false)
      : type(std::move(t)), value(std::move(v)), is_string_literal(is_str) {
  }
};

inline auto operator<<(std::ostream& os, const Literal& literal)
    -> std::ostream& {
  return os << literal.ToString();
}

}  // namespace lyra::common

// fmt::formatter support
template <>
struct fmt::formatter<lyra::common::Literal> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::Literal& literal, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", literal.ToString());
  }
};
