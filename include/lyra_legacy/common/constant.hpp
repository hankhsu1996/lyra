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

/// Compile-time constant value.
///
/// Represents an immutable value known at compile time. Used in MIR
/// (ConstantExpression) and LIR (ConstantRef). Constants are interned for
/// deduplication.
///
/// Distinct from RuntimeValue which represents execution-time state in the
/// interpreter. The conversion from Constant to RuntimeValue at the interpreter
/// boundary marks the transition from program description to execution state.
struct Constant {
  common::Type type{};
  common::ValueStorage value{};
  bool is_string_literal = false;

  static auto Void() -> Constant {
    return {common::Type::Void(), ValueStorage::Void()};
  }

  static auto Bool(bool b) -> Constant {
    return {common::Type::Bool(), ValueStorage(static_cast<int64_t>(b))};
  }

  static auto Int(int32_t v) -> Constant {
    // Direct cast to int64_t automatically sign extends correctly
    return {common::Type::Int(), ValueStorage(static_cast<int64_t>(v))};
  }

  static auto UInt(uint32_t v) -> Constant {
    return {common::Type::UInt(), ValueStorage(static_cast<int64_t>(v))};
  }

  static auto LongInt(int64_t v) -> Constant {
    return {common::Type::LongInt(), ValueStorage(v)};
  }

  static auto ULongInt(uint64_t v) -> Constant {
    return {common::Type::ULongInt(), ValueStorage(static_cast<int64_t>(v))};
  }

  static auto IntegralSigned(int64_t v, size_t width) -> Constant {
    return {common::Type::IntegralSigned(width), ValueStorage(v)};
  }

  static auto IntegralUnsigned(uint64_t v, size_t width) -> Constant {
    return {
        common::Type::IntegralUnsigned(width),
        ValueStorage(static_cast<int64_t>(v))};
  }

  // Factory for wide integral values (>64 bits)
  static auto IntegralWide(WideBit value, size_t width, bool is_signed)
      -> Constant {
    return {
        is_signed ? common::Type::IntegralSigned(width)
                  : common::Type::IntegralUnsigned(width),
        ValueStorage(std::move(value))};
  }

  static auto String(std::string v) -> Constant {
    return Constant{common::Type::String(), ValueStorage(std::move(v)), true};
  }

  static auto Real(double v) -> Constant {
    return {common::Type::Real(), ValueStorage(v)};
  }

  static auto ShortReal(float v) -> Constant {
    return {common::Type::ShortReal(), ValueStorage(v)};
  }

  // Generic factory for creating a constant from type and value.
  // Used by MIR-to-LIR when handling BitPackedStringExpression.
  static auto FromTypeAndValue(
      common::Type type, ValueStorage value, bool is_string_literal = false)
      -> Constant {
    return Constant{std::move(type), std::move(value), is_string_literal};
  }

  auto operator==(const Constant& other) const -> bool = default;

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

  /// Returns true if this constant represents a string literal in source.
  /// Checks both the explicit flag and type, since string literals may be
  /// stored as either Type::kString or as packed integers (bit[N]) depending
  /// on context. See docs/string-types.md for details.
  [[nodiscard]] auto IsStringLiteral() const -> bool {
    return is_string_literal || type.kind == Type::Kind::kString;
  }

 private:
  Constant(common::Type t, ValueStorage v, bool is_str = false)
      : type(std::move(t)), value(std::move(v)), is_string_literal(is_str) {
  }
};

inline auto operator<<(std::ostream& os, const Constant& constant)
    -> std::ostream& {
  return os << constant.ToString();
}

}  // namespace lyra::common

// fmt::formatter support
template <>
struct fmt::formatter<lyra::common::Constant> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::common::Constant& constant, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", constant.ToString());
  }
};
