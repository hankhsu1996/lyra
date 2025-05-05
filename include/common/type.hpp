#pragma once

#include <string>
#include <variant>

#include <fmt/core.h>

namespace lyra::common {

struct TwoStateData {
  size_t bit_width;
  bool is_signed;

  auto operator==(const TwoStateData& other) const -> bool = default;
};

struct Type {
  enum class Kind { kVoid, kTwoState, kString };

  Kind kind{};
  std::variant<std::monostate, TwoStateData> data{};

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
    return TwoStateSigned(1);
  }

  static auto String() -> Type {
    return Type{.kind = Kind::kString};
  }

  auto operator==(const Type& other) const -> bool = default;

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      case Kind::kVoid:
        return "void";
      case Kind::kTwoState:
        return fmt::format(
            "bit[{}] {}", std::get<TwoStateData>(data).bit_width,
            std::get<TwoStateData>(data).is_signed ? "signed" : "unsigned");
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
