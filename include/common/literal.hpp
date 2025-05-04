#pragma once

#include <ostream>
#include <string>
#include <utility>

#include <fmt/core.h>
#include <fmt/format.h>

#include "common/type.hpp"
#include "common/value_storage.hpp"

namespace lyra::common {

struct Literal {
  common::Type type{};
  common::ValueStorage value{};

  Literal() = default;

  Literal(common::Type t, ValueStorage v)
      : type(std::move(t)), value(std::move(v)) {
  }

  static auto Void() -> Literal {
    return {common::Type::Void(), ValueStorage::Void()};
  }

  static auto Bool(bool b) -> Literal {
    return {common::Type::Bool(), ValueStorage(static_cast<int64_t>(b))};
  }

  static auto Int(int32_t v) -> Literal {
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

  [[nodiscard]] auto ToString() const -> std::string {
    return value.ToString();
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
