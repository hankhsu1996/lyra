#pragma once

#include <functional>
#include <ostream>
#include <string>
#include <variant>

#include <fmt/core.h>
#include <slang/ast/types/Type.h>

#include "lyra/common/meta_util.hpp"

namespace lyra::common {

struct TwoStateData {
  size_t bit_width;
  bool is_signed;

  auto operator==(const TwoStateData& other) const -> bool = default;

  [[nodiscard]] auto Hash() const -> std::size_t {
    std::size_t h = 0;
    h ^= std::hash<size_t>{}(bit_width) + 0x9e3779b9 + (h << 6) + (h >> 2);
    h ^= std::hash<bool>{}(is_signed) + 0x9e3779b9 + (h << 6) + (h >> 2);
    return h;
  }
};

struct Type {
  enum class Kind { kVoid, kTwoState, kReal, kString };

  Kind kind{};
  std::variant<std::monostate, TwoStateData> data{};

  static auto FromSlang(const slang::ast::Type& type) -> Type {
    if (type.isString()) {
      return Type{.kind = Kind::kString};
    }
    if (type.isFloating()) {
      return Type::Real();
    }
    if (type.isIntegral()) {
      if (type.isSigned()) {
        return Type::TwoStateSigned(type.getBitWidth());
      }
      return Type::TwoStateUnsigned(type.getBitWidth());
    }
    throw std::runtime_error(
        fmt::format("Unsupported type: {}", type.toString()));
  }

  static auto Void() -> Type {
    return Type{.kind = Kind::kVoid};
  }

  static auto TwoState(size_t bit_width, bool is_signed) -> Type {
    return Type{
        .kind = Kind::kTwoState,
        .data = TwoStateData{.bit_width = bit_width, .is_signed = is_signed}};
  }

  static auto TwoStateSigned(size_t bit_width) -> Type {
    return TwoState(bit_width, true);
  }

  static auto TwoStateUnsigned(size_t bit_width) -> Type {
    return TwoState(bit_width, false);
  }

  static auto Int() -> Type {
    return TwoStateSigned(32);
  }

  static auto UInt() -> Type {
    return TwoStateUnsigned(32);
  }

  static auto LongInt() -> Type {
    return TwoStateSigned(64);
  }

  static auto ULongInt() -> Type {
    return TwoStateUnsigned(64);
  }

  static auto Bool() -> Type {
    return TwoStateUnsigned(1);
  }

  static auto String() -> Type {
    return Type{.kind = Kind::kString};
  }

  static auto Real() -> Type {
    return Type{.kind = Kind::kReal};
  }

  auto operator==(const Type& other) const -> bool = default;

  [[nodiscard]] auto Hash() const -> std::size_t {
    std::size_t h = 0;

    std::size_t kind_hash = std::hash<int>{}(static_cast<int>(kind));
    h ^= kind_hash + 0x9e3779b9 + (h << 6) + (h >> 2);

    std::size_t data_hash = std::visit(
        [](const auto& val) -> std::size_t {
          using T = std::decay_t<decltype(val)>;
          if constexpr (std::is_same_v<T, std::monostate>) {
            return 0;
          } else if constexpr (requires { val.Hash(); }) {
            return val.Hash();
          } else {
            static_assert(
                kAlwaysFalse<T>, "Unhandled variant type in Type::Hash()");
            return 0;
          }
        },
        data);

    h ^= data_hash + 0x9e3779b9 + (h << 6) + (h >> 2);

    return h;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      case Kind::kVoid:
        return "void";
      case Kind::kTwoState:
        return fmt::format(
            "bit[{}] {}", std::get<TwoStateData>(data).bit_width,
            std::get<TwoStateData>(data).is_signed ? "signed" : "unsigned");
      case Kind::kReal:
        return "real";
      case Kind::kString:
        return "string";
    }
  }
};

inline auto ToString(Type::Kind kind) -> std::string {
  switch (kind) {
    case Type::Kind::kVoid:
      return "void";
    case Type::Kind::kTwoState:
      return "bit";
    case Type::Kind::kReal:
      return "real";
    case Type::Kind::kString:
      return "string";
  }
}

inline auto operator<<(std::ostream& os, Type::Kind kind) -> std::ostream& {
  return os << ToString(kind);
}

inline auto operator<<(std::ostream& os, Type type) -> std::ostream& {
  return os << type.ToString();
}

}  // namespace lyra::common

template <>
struct fmt::formatter<lyra::common::Type::Kind> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::Type::Kind& type, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", lyra::common::ToString(type));
  }
};

template <>
struct fmt::formatter<lyra::common::Type> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::Type& type, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", type.ToString());
  }
};
