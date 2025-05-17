#pragma once

#include <deque>
#include <functional>
#include <string>
#include <string_view>
#include <unordered_set>

#include <fmt/core.h>

#include "lyra/common/literal.hpp"

namespace lyra::lir {

struct TempSymbol {
  std::string name;
};

struct TempRef {
  TempSymbol* ptr;

  [[nodiscard]] auto operator*() const -> TempSymbol& {
    return *ptr;
  }

  [[nodiscard]] auto operator->() const -> TempSymbol* {
    return ptr;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return ptr->name;
  }

  [[nodiscard]] friend auto operator==(const TempRef& a, const TempRef& b)
      -> bool {
    return a.ptr == b.ptr;
  }
};

struct LabelRef {
  const std::string* ptr;

  [[nodiscard]] auto operator*() const -> const std::string& {
    return *ptr;
  }

  [[nodiscard]] auto operator->() const -> const std::string* {
    return ptr;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return *ptr;
  }

  [[nodiscard]] friend auto operator==(const LabelRef& a, const LabelRef& b)
      -> bool {
    return a.ptr == b.ptr;
  }
};

struct LiteralRef {
  const common::Literal* ptr;

  [[nodiscard]] auto operator*() const -> const common::Literal& {
    return *ptr;
  }

  [[nodiscard]] auto operator->() const -> const common::Literal* {
    return ptr;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return ptr->ToString();
  }

  [[nodiscard]] friend auto operator==(const LiteralRef& a, const LiteralRef& b)
      -> bool {
    return a.ptr == b.ptr;
  }
};

class LirContext {
 public:
  auto AllocateTemp(std::string name) -> TempRef;
  auto InternLabel(std::string_view name) -> LabelRef;
  auto InternLiteral(const common::Literal& literal) -> LiteralRef;

 private:
  std::deque<TempSymbol> temp_storage_;
  std::unordered_set<std::string> label_pool_;
  std::deque<common::Literal> literal_storage_;

  struct LiteralPtrHash {
    auto operator()(const common::Literal* ptr) const -> std::size_t;
  };

  struct LiteralPtrEqual {
    auto operator()(const common::Literal* a, const common::Literal* b) const
        -> bool;
  };

  std::unordered_set<const common::Literal*, LiteralPtrHash, LiteralPtrEqual>
      literal_set_;
};

}  // namespace lyra::lir

// Hash support
namespace std {
template <>
struct hash<lyra::lir::TempRef> {
  auto operator()(const lyra::lir::TempRef& ref) const -> std::size_t {
    return std::hash<void*>{}(ref.ptr);
  }
};

template <>
struct hash<lyra::lir::LabelRef> {
  auto operator()(const lyra::lir::LabelRef& ref) const -> std::size_t {
    return std::hash<const void*>{}(ref.ptr);
  }
};

template <>
struct hash<lyra::lir::LiteralRef> {
  auto operator()(const lyra::lir::LiteralRef& ref) const -> std::size_t {
    return std::hash<const void*>{}(ref.ptr);
  }
};
}  // namespace std

// fmt formatter support
template <>
struct fmt::formatter<lyra::lir::TempRef> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::TempRef& ref, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", ref.ToString());
  }
};

template <>
struct fmt::formatter<lyra::lir::LabelRef> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::LabelRef& ref, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", ref.ToString());
  }
};

template <>
struct fmt::formatter<lyra::lir::LiteralRef> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::LiteralRef& ref, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", ref.ToString());
  }
};
