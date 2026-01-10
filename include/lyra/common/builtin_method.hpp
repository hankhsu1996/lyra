#pragma once

#include <array>
#include <cstdint>
#include <string_view>

namespace lyra::common {

/// Built-in type kinds that have methods
/// Maps to Type::Kind but only for types with built-in methods
enum class BuiltinTypeKind : uint8_t {
  kDynamicArray,
  kEnum,
  kString,
  // Future: kQueue, kAssocArray
};

/// Return type for built-in methods
enum class BuiltinMethodReturnType : uint8_t {
  kVoid,
  kInt,     // 32-bit signed integer
  kString,  // string type
  kSelf,    // Same type as receiver (for enum next/prev)
};

/// Category for grouping related methods
enum class BuiltinMethodCategory : uint8_t {
  kArrayQuery,   // size()
  kArrayMutate,  // delete()
  kEnumNext,     // next()
  kEnumPrev,     // prev()
  kEnumName,     // name()
};

/// Metadata for a built-in method
struct BuiltinMethodInfo {
  std::string_view name;
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
    .name = "size",
    .receiver_type = BuiltinTypeKind::kDynamicArray,
    .category = BuiltinMethodCategory::kArrayQuery,
    .return_type = BuiltinMethodReturnType::kInt,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = ".size()",
  },
  {
    .name = "delete",
    .receiver_type = BuiltinTypeKind::kDynamicArray,
    .category = BuiltinMethodCategory::kArrayMutate,
    .return_type = BuiltinMethodReturnType::kVoid,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = ".clear()",
  },

  // Enum methods (require special handling for switch generation)
  {
    .name = "next",
    .receiver_type = BuiltinTypeKind::kEnum,
    .category = BuiltinMethodCategory::kEnumNext,
    .return_type = BuiltinMethodReturnType::kSelf,
    .min_args = 0,
    .max_args = 1,
    .cpp_expr = "",  // Special handling: generate switch
  },
  {
    .name = "prev",
    .receiver_type = BuiltinTypeKind::kEnum,
    .category = BuiltinMethodCategory::kEnumPrev,
    .return_type = BuiltinMethodReturnType::kSelf,
    .min_args = 0,
    .max_args = 1,
    .cpp_expr = "",  // Special handling: generate switch
  },
  {
    .name = "name",
    .receiver_type = BuiltinTypeKind::kEnum,
    .category = BuiltinMethodCategory::kEnumName,
    .return_type = BuiltinMethodReturnType::kString,
    .min_args = 0,
    .max_args = 0,
    .cpp_expr = "",  // Special handling: generate switch
  },
});
// clang-format on

/// Find built-in method by receiver type and name
/// Returns nullptr if not found
constexpr auto FindBuiltinMethod(BuiltinTypeKind type, std::string_view name)
    -> const BuiltinMethodInfo* {
  for (const auto& info : kBuiltinMethods) {
    if (info.receiver_type == type && info.name == name) {
      return &info;
    }
  }
  return nullptr;
}

/// Check if a method requires special handling (no direct C++ mapping)
constexpr auto RequiresSpecialHandling(const BuiltinMethodInfo& info) -> bool {
  return info.cpp_expr.empty();
}

}  // namespace lyra::common
