#pragma once

#include <cassert>
#include <cstdint>
#include <ostream>
#include <string>
#include <variant>

#include <fmt/core.h>

#include "lyra/common/meta_util.hpp"

namespace lyra::common {

template <typename... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template <typename Variant, typename... Matchers>
auto Match(Variant&& v, Matchers&&... matchers) {
  return std::visit(
      Overloaded{std::forward<Matchers>(matchers)...},
      std::forward<Variant>(v));
}

class ValueStorage {
 public:
  using Storage = std::variant<std::monostate, int64_t, std::string>;

  ValueStorage() = default;
  explicit ValueStorage(std::monostate v) : value_(v) {
  }
  explicit ValueStorage(int64_t v) : value_(v) {
  }
  explicit ValueStorage(std::string v) : value_(std::move(v)) {
  }

  static auto Void() -> ValueStorage {
    return ValueStorage(std::monostate{});
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return Match(
        value_, [](std::monostate) { return std::string{}; },
        [](int64_t v) { return std::to_string(v); },
        [](const std::string& v) { return "\"" + v + "\""; });
  }

  template <typename T>
  [[nodiscard]] auto Is() const -> bool {
    return std::holds_alternative<T>(value_);
  }

  [[nodiscard]] auto IsVoid() const -> bool {
    return Is<std::monostate>();
  }

  [[nodiscard]] auto IsInt64() const -> bool {
    return Is<int64_t>();
  }

  [[nodiscard]] auto IsString() const -> bool {
    return Is<std::string>();
  }

  template <typename T>
  [[nodiscard]] auto As() const -> const T& {
    assert(Is<T>() && "Bad variant access");
    return std::get<T>(value_);
  }

  [[nodiscard]] auto AsInt64() const -> int64_t {
    return As<int64_t>();
  }

  [[nodiscard]] auto AsString() const -> const std::string& {
    return As<std::string>();
  }

  auto Raw() -> auto& {
    return value_;
  }

  [[nodiscard]] auto Raw() const -> const auto& {
    return value_;
  }

  auto operator==(const ValueStorage& other) const -> bool = default;

  [[nodiscard]] auto Hash() const -> std::size_t {
    return std::visit(
        [](const auto& val) -> std::size_t {
          using T = std::decay_t<decltype(val)>;
          if constexpr (std::is_same_v<T, std::monostate>) {
            return 0;
          } else if constexpr (std::is_same_v<T, int64_t>) {
            return std::hash<int64_t>{}(val);
          } else if constexpr (std::is_same_v<T, std::string>) {
            return std::hash<std::string>{}(val);
          } else {
            static_assert(
                kAlwaysFalse<T>, "Unhandled type in ValueStorage::Hash()");
          }
        },
        value_);
  }

 private:
  Storage value_;
};

inline auto operator<<(std::ostream& os, const ValueStorage& value)
    -> std::ostream& {
  return os << value.ToString();
}

}  // namespace lyra::common

template <>
struct fmt::formatter<lyra::common::ValueStorage> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) -> ParseContext::iterator {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::ValueStorage& value, FormatContext& ctx)
      -> FormatContext::iterator {
    return fmt::format_to(ctx.out(), "{}", value.ToString());
  }
};
