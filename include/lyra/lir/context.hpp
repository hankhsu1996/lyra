#pragma once

#include <deque>
#include <functional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include <fmt/core.h>

#include "lyra/common/constant.hpp"

namespace lyra::lir {

using TempId = uint32_t;

struct TempSymbol {
  std::string name;
  common::Type type;
};

struct TempRef {
  TempId id;
  TempSymbol* ptr;  // Cached pointer for fast access

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
    return a.id == b.id;
  }
};

using LabelId = uint32_t;

struct LabelRef {
  LabelId id;
  const std::string* ptr;  // Cached pointer for fast access

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
    return a.id == b.id;
  }
};

struct ConstantRef {
  const common::Constant* ptr;

  [[nodiscard]] auto operator*() const -> const common::Constant& {
    return *ptr;
  }

  [[nodiscard]] auto operator->() const -> const common::Constant* {
    return ptr;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return ptr->ToString();
  }

  [[nodiscard]] friend auto operator==(
      const ConstantRef& a, const ConstantRef& b) -> bool {
    return a.ptr == b.ptr;
  }
};

class LirContext {
 public:
  auto AllocateTemp(std::string name, common::Type type) -> TempRef;
  auto InternLabel(std::string_view name) -> LabelRef;
  auto InternConstant(const common::Constant& constant) -> ConstantRef;
  auto InternType(const common::Type& type) -> const common::Type*;

 private:
  std::deque<TempSymbol> temp_storage_;
  std::deque<std::string> label_storage_;
  std::unordered_map<std::string, LabelId> label_index_;
  std::deque<common::Constant> constant_storage_;
  std::deque<common::Type> type_storage_;

  struct ConstantPtrHash {
    auto operator()(const common::Constant* ptr) const -> std::size_t;
  };

  struct ConstantPtrEqual {
    auto operator()(const common::Constant* a, const common::Constant* b) const
        -> bool;
  };

  std::unordered_set<const common::Constant*, ConstantPtrHash, ConstantPtrEqual>
      constant_set_;
};

}  // namespace lyra::lir

// Hash support
namespace std {
template <>
struct hash<lyra::lir::TempRef> {
  auto operator()(const lyra::lir::TempRef& ref) const -> std::size_t {
    return std::hash<lyra::lir::TempId>{}(ref.id);
  }
};

template <>
struct hash<lyra::lir::LabelRef> {
  auto operator()(const lyra::lir::LabelRef& ref) const -> std::size_t {
    return std::hash<lyra::lir::LabelId>{}(ref.id);
  }
};

template <>
struct hash<lyra::lir::ConstantRef> {
  auto operator()(const lyra::lir::ConstantRef& ref) const -> std::size_t {
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
struct fmt::formatter<lyra::lir::ConstantRef> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::ConstantRef& ref, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", ref.ToString());
  }
};
