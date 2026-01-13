#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <string_view>

namespace lyra::common {

/// Built-in method kinds for type-safe dispatch (no string matching at runtime)
enum class BuiltinMethod : uint8_t {
  // Container methods (array, queue)
  kSize,
  kDelete,
  // Queue-only methods
  kPushBack,
  kPushFront,
  kPopBack,
  kPopFront,
  kInsert,
  // Enum methods
  kFirst,
  kLast,
  kNext,
  kPrev,
  kName,
};

inline auto ToString(BuiltinMethod method) -> std::string_view {
  switch (method) {
    case BuiltinMethod::kSize:
      return "size";
    case BuiltinMethod::kDelete:
      return "delete";
    case BuiltinMethod::kPushBack:
      return "push_back";
    case BuiltinMethod::kPushFront:
      return "push_front";
    case BuiltinMethod::kPopBack:
      return "pop_back";
    case BuiltinMethod::kPopFront:
      return "pop_front";
    case BuiltinMethod::kInsert:
      return "insert";
    case BuiltinMethod::kFirst:
      return "first";
    case BuiltinMethod::kLast:
      return "last";
    case BuiltinMethod::kNext:
      return "next";
    case BuiltinMethod::kPrev:
      return "prev";
    case BuiltinMethod::kName:
      return "name";
  }
  return "unknown";
}

/// Parse method name string to BuiltinMethod enum.
/// Returns nullopt for unknown methods.
inline auto ParseBuiltinMethod(std::string_view name)
    -> std::optional<BuiltinMethod> {
  if (name == "size") {
    return BuiltinMethod::kSize;
  }
  if (name == "delete") {
    return BuiltinMethod::kDelete;
  }
  if (name == "push_back") {
    return BuiltinMethod::kPushBack;
  }
  if (name == "push_front") {
    return BuiltinMethod::kPushFront;
  }
  if (name == "pop_back") {
    return BuiltinMethod::kPopBack;
  }
  if (name == "pop_front") {
    return BuiltinMethod::kPopFront;
  }
  if (name == "insert") {
    return BuiltinMethod::kInsert;
  }
  if (name == "first") {
    return BuiltinMethod::kFirst;
  }
  if (name == "last") {
    return BuiltinMethod::kLast;
  }
  if (name == "next") {
    return BuiltinMethod::kNext;
  }
  if (name == "prev") {
    return BuiltinMethod::kPrev;
  }
  if (name == "name") {
    return BuiltinMethod::kName;
  }
  return std::nullopt;
}

/// Built-in type kinds that have methods
/// Maps to Type::Kind but only for types with built-in methods
enum class BuiltinTypeKind : uint8_t {
  kDynamicArray,
  kQueue,
  kEnum,
  kString,
  // Future: kAssocArray
};

/// Return type for built-in methods
enum class BuiltinMethodReturnType : uint8_t {
  kVoid,
  kInt,      // 32-bit signed integer
  kString,   // string type
  kSelf,     // Same type as receiver (for enum next/prev)
  kElement,  // Element type of container (for queue pop methods)
};

/// Category for grouping related methods
enum class BuiltinMethodCategory : uint8_t {
  kArrayQuery,   // size()
  kArrayMutate,  // delete()
  kQueuePush,    // push_front(), push_back()
  kQueuePop,     // pop_front(), pop_back() - returns element
  kQueueInsert,  // insert(index, item)
  kEnumNext,     // next()
  kEnumPrev,     // prev()
  kEnumName,     // name()
};

/// Metadata for a built-in method
struct BuiltinMethodInfo {
  BuiltinMethod method;
  BuiltinTypeKind receiver_type;
  BuiltinMethodCategory category;
  BuiltinMethodReturnType return_type;
  uint8_t min_args;
  uint8_t max_args;
  std::string_view
      cpp_expr;  // C++ expression suffix (empty = special handling)
};

// clang-format off
inline constexpr std::array kBuiltinMethods = std::to_array<BuiltinMethodInfo>({
  // Dynamic array methods
  {
    .method = BuiltinMethod::kSize,
    .receiver_type = BuiltinTypeKind::kDynamicArray,
    .category = BuiltinMethodCategory::kArrayQuery,
    .return_type = BuiltinMethodReturnType::kInt,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = ".size()",
  },
  {
    .method = BuiltinMethod::kDelete,
    .receiver_type = BuiltinTypeKind::kDynamicArray,
    .category = BuiltinMethodCategory::kArrayMutate,
    .return_type = BuiltinMethodReturnType::kVoid,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = ".clear()",
  },

  // Queue methods
  {
    .method = BuiltinMethod::kSize,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kArrayQuery,
    .return_type = BuiltinMethodReturnType::kInt,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = ".size()",
  },
  {
    .method = BuiltinMethod::kPushBack,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kQueuePush,
    .return_type = BuiltinMethodReturnType::kVoid,
    .min_args = 1,
    .max_args = 1,
    .cpp_expr = "",  // Special handling: needs argument
  },
  {
    .method = BuiltinMethod::kPushFront,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kQueuePush,
    .return_type = BuiltinMethodReturnType::kVoid,
    .min_args = 1,
    .max_args = 1,
    .cpp_expr = "",  // Special handling: needs argument
  },
  {
    .method = BuiltinMethod::kPopBack,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kQueuePop,
    .return_type = BuiltinMethodReturnType::kElement,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = "",  // Special handling: removes and returns
  },
  {
    .method = BuiltinMethod::kPopFront,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kQueuePop,
    .return_type = BuiltinMethodReturnType::kElement,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = "",  // Special handling: removes and returns
  },
  {
    .method = BuiltinMethod::kInsert,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kQueueInsert,
    .return_type = BuiltinMethodReturnType::kVoid,
    .min_args = 2,
    .max_args = 2,
    .cpp_expr = "",  // Special handling: needs index and item
  },
  {
    .method = BuiltinMethod::kDelete,
    .receiver_type = BuiltinTypeKind::kQueue,
    .category = BuiltinMethodCategory::kArrayMutate,
    .return_type = BuiltinMethodReturnType::kVoid,
    .min_args = 0,
    .max_args = 1,  // delete() or delete(index)
    .cpp_expr = "",  // Special handling: two forms
  },

  // Enum methods (require special handling for switch generation)
  {
    .method = BuiltinMethod::kNext,
    .receiver_type = BuiltinTypeKind::kEnum,
    .category = BuiltinMethodCategory::kEnumNext,
    .return_type = BuiltinMethodReturnType::kSelf,
    .min_args = 0,
    .max_args = 1,
    .cpp_expr = "",  // Special handling: generate switch
  },
  {
    .method = BuiltinMethod::kPrev,
    .receiver_type = BuiltinTypeKind::kEnum,
    .category = BuiltinMethodCategory::kEnumPrev,
    .return_type = BuiltinMethodReturnType::kSelf,
    .min_args = 0,
    .max_args = 1,
    .cpp_expr = "",  // Special handling: generate switch
  },
  {
    .method = BuiltinMethod::kName,
    .receiver_type = BuiltinTypeKind::kEnum,
    .category = BuiltinMethodCategory::kEnumName,
    .return_type = BuiltinMethodReturnType::kString,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = "",  // Special handling: generate switch
  },
});
// clang-format on

/// Find built-in method info by enum and receiver type.
/// Returns nullptr if the method is not valid for the given type.
constexpr auto GetMethodInfo(BuiltinMethod method, BuiltinTypeKind type)
    -> const BuiltinMethodInfo* {
  for (const auto& info : kBuiltinMethods) {
    if (info.method == method && info.receiver_type == type) {
      return &info;
    }
  }
  return nullptr;
}

/// Find built-in method by receiver type and name (for AST→MIR parsing).
/// Returns nullptr if not found.
constexpr auto FindBuiltinMethod(BuiltinTypeKind type, std::string_view name)
    -> const BuiltinMethodInfo* {
  auto method = ParseBuiltinMethod(name);
  if (!method) {
    return nullptr;
  }
  return GetMethodInfo(*method, type);
}

/// Check if a method requires special handling (no direct C++ mapping)
constexpr auto RequiresSpecialHandling(const BuiltinMethodInfo& info) -> bool {
  return info.cpp_expr.empty();
}

}  // namespace lyra::common
