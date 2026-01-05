#pragma once

#include <iostream>
#include <ostream>
#include <string>
#include <utility>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/value_storage.hpp"

namespace lyra::common {

struct Literal {
  common::Type type{};
  common::ValueStorage value{};

  static auto Void() -> Literal {
    return {common::Type::Void(), ValueStorage::Void()};
  }

  static auto Bool(bool b) -> Literal {
    return {common::Type::Bool(), ValueStorage(static_cast<int64_t>(b))};
  }

  static auto Int(int32_t v) -> Literal {
    int64_t extended = common::SignExtend(static_cast<uint64_t>(v), 32);
    return {common::Type::Int(), ValueStorage(extended)};
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

  static auto TwoStateSigned(int64_t v, size_t width) -> Literal {
    return {common::Type::TwoStateSigned(width), ValueStorage(v)};
  }

  static auto TwoStateUnsigned(uint64_t v, size_t width) -> Literal {
    return {
        common::Type::TwoStateUnsigned(width),
        ValueStorage(static_cast<int64_t>(v))};
  }

  static auto String(std::string v) -> Literal {
    return {common::Type::String(), ValueStorage(std::move(v))};
  }

  static auto Real(double v) -> Literal {
    return {common::Type::Real(), ValueStorage(v)};
  }

  static auto ShortReal(float v) -> Literal {
    return {common::Type::ShortReal(), ValueStorage(v)};
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
  Literal(common::Type t, ValueStorage v)
      : type(std::move(t)), value(std::move(v)) {
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
