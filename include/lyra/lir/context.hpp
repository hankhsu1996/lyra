#pragma once

#include <cstdint>
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

// Hint IDs for common temp purposes - for readable debug output
enum class HintId : uint16_t {
  kGeneric = 0,  // %t_<id>
  kPtr,          // %ptr_<id>
  kLhs,          // %lhs_<id>
  kRhs,          // %rhs_<id>
  kResult,       // %result_<id>
  kCond,         // %cond_<id>
  kIndex,        // %idx_<id>
  kCounter,      // %cnt_<id>
  kTernary,      // %ternary_<id>
  kSlice,        // %slice_<id>
  kValue,        // %val_<id>
  kArg,          // %arg_<id>
  kRet,          // %ret_<id>
  kBound,        // %bound_<id>
  kOffset,       // %offset_<id>
  kWidth,        // %width_<id>
  kElem,         // %elem_<id>
};

// Metadata for a temp - stored per Process/Function
struct TempMeta {
  const common::Type* type;  // Interned pointer
  HintId hint;               // For readable debug output
};

// Get hint prefix for debug output
inline auto GetHintPrefix(HintId hint) -> std::string_view {
  switch (hint) {
    case HintId::kGeneric:
      return "t";
    case HintId::kPtr:
      return "ptr";
    case HintId::kLhs:
      return "lhs";
    case HintId::kRhs:
      return "rhs";
    case HintId::kResult:
      return "result";
    case HintId::kCond:
      return "cond";
    case HintId::kIndex:
      return "idx";
    case HintId::kCounter:
      return "cnt";
    case HintId::kTernary:
      return "ternary";
    case HintId::kSlice:
      return "slice";
    case HintId::kValue:
      return "val";
    case HintId::kArg:
      return "arg";
    case HintId::kRet:
      return "ret";
    case HintId::kBound:
      return "bound";
    case HintId::kOffset:
      return "offset";
    case HintId::kWidth:
      return "width";
    case HintId::kElem:
      return "elem";
  }
  return "t";  // Fallback
}

struct TempRef {
  TempId id;
  HintId hint;

  [[nodiscard]] auto ToString() const -> std::string {
    return fmt::format("%{}_{}", GetHintPrefix(hint), id);
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
  // Allocate temp with explicit per-unit ID (for vector-based TempTable)
  static auto AllocateTempWithId(TempId id, HintId hint, common::Type type)
      -> TempRef;
  auto InternLabel(std::string_view name) -> LabelRef;
  auto InternConstant(const common::Constant& constant) -> ConstantRef;
  auto InternType(const common::Type& type) -> const common::Type*;

 private:
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
