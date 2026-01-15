#pragma once

#include <cassert>
#include <cstdint>
#include <format>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/range.hpp"

namespace lyra {

struct TypeId {
  uint32_t value = 0;

  auto operator==(const TypeId&) const -> bool = default;
  auto operator<=>(const TypeId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, TypeId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr TypeId kInvalidTypeId{UINT32_MAX};

// Forward declaration for recursive types
class Type;

enum class TypeKind {
  kVoid,
  kIntegral,
  kUnpackedArray,
  kUnpackedStruct,
};

struct IntegralInfo {
  uint32_t bit_width;
  bool is_signed;
  bool is_four_state;

  auto operator==(const IntegralInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const IntegralInfo& info) -> H {
    return H::combine(
        std::move(h), info.bit_width, info.is_signed, info.is_four_state);
  }
};

struct UnpackedArrayInfo {
  TypeId element_type;
  ConstantRange range;

  auto operator==(const UnpackedArrayInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const UnpackedArrayInfo& info) -> H {
    return H::combine(std::move(h), info.element_type, info.range);
  }
};

struct StructField {
  std::string name;
  TypeId type;

  auto operator==(const StructField&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const StructField& field) -> H {
    return H::combine(std::move(h), field.name, field.type);
  }
};

struct UnpackedStructInfo {
  std::string name;
  std::vector<StructField> fields;

  auto operator==(const UnpackedStructInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const UnpackedStructInfo& info) -> H {
    return H::combine(std::move(h), info.name, info.fields);
  }
};

using TypePayload = std::variant<
    std::monostate, IntegralInfo, UnpackedArrayInfo, UnpackedStructInfo>;

struct TypeKey {
  TypeKind kind = TypeKind::kVoid;
  TypePayload payload;

  auto operator==(const TypeKey&) const -> bool = default;
};

class TypeArena;

class Type {
  friend class TypeArena;

 public:
  // Factory functions
  static auto Void() -> Type;
  static auto Integral(uint32_t bit_width, bool is_signed, bool is_four_state)
      -> Type;
  static auto UnpackedArray(TypeId element, ConstantRange range) -> Type;
  static auto UnpackedStruct(std::string name, std::vector<StructField> fields)
      -> Type;

  [[nodiscard]] auto Kind() const -> TypeKind {
    return kind_;
  }

  // Kind-specific accessors (assert if wrong kind)
  [[nodiscard]] auto AsIntegral() const -> const IntegralInfo&;
  [[nodiscard]] auto AsUnpackedArray() const -> const UnpackedArrayInfo&;
  [[nodiscard]] auto AsUnpackedStruct() const -> const UnpackedStructInfo&;

  auto operator==(const Type& other) const -> bool = default;

 private:
  TypeKind kind_ = TypeKind::kVoid;
  TypePayload payload_;
};

inline auto Type::Void() -> Type {
  Type t;
  t.kind_ = TypeKind::kVoid;
  t.payload_ = std::monostate{};
  return t;
}

inline auto Type::Integral(
    uint32_t bit_width, bool is_signed, bool is_four_state) -> Type {
  Type t;
  t.kind_ = TypeKind::kIntegral;
  t.payload_ = IntegralInfo{
      .bit_width = bit_width,
      .is_signed = is_signed,
      .is_four_state = is_four_state};
  return t;
}

inline auto Type::UnpackedArray(TypeId element, ConstantRange range) -> Type {
  Type t;
  t.kind_ = TypeKind::kUnpackedArray;
  t.payload_ = UnpackedArrayInfo{.element_type = element, .range = range};
  return t;
}

inline auto Type::UnpackedStruct(
    std::string name, std::vector<StructField> fields) -> Type {
  Type t;
  t.kind_ = TypeKind::kUnpackedStruct;
  t.payload_ =
      UnpackedStructInfo{.name = std::move(name), .fields = std::move(fields)};
  return t;
}

inline auto Type::AsIntegral() const -> const IntegralInfo& {
  assert(kind_ == TypeKind::kIntegral);
  return std::get<IntegralInfo>(payload_);
}

inline auto Type::AsUnpackedArray() const -> const UnpackedArrayInfo& {
  assert(kind_ == TypeKind::kUnpackedArray);
  return std::get<UnpackedArrayInfo>(payload_);
}

inline auto Type::AsUnpackedStruct() const -> const UnpackedStructInfo& {
  assert(kind_ == TypeKind::kUnpackedStruct);
  return std::get<UnpackedStructInfo>(payload_);
}

inline auto ToString(TypeKind kind) -> std::string {
  switch (kind) {
    case TypeKind::kVoid:
      return "void";
    case TypeKind::kIntegral:
      return "integral";
    case TypeKind::kUnpackedArray:
      return "unpacked_array";
    case TypeKind::kUnpackedStruct:
      return "unpacked_struct";
  }
  return "unknown";
}

inline auto ToString(const Type& type) -> std::string {
  switch (type.Kind()) {
    case TypeKind::kVoid:
      return "void";
    case TypeKind::kIntegral: {
      const auto& info = type.AsIntegral();
      return std::format(
          "{}{}[{}]", info.is_four_state ? "logic" : "bit",
          info.is_signed ? " signed" : "", info.bit_width);
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return std::format(
          "type#{}[{}:{}]", info.element_type.value, info.range.lower,
          info.range.upper);
    }
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      return std::format("struct {}", info.name);
    }
  }
  return "unknown";
}

}  // namespace lyra

template <>
struct std::formatter<lyra::TypeKind> : std::formatter<std::string> {
  auto format(lyra::TypeKind kind, std::format_context& ctx) const {
    return std::formatter<std::string>::format(lyra::ToString(kind), ctx);
  }
};

template <>
struct std::formatter<lyra::Type> : std::formatter<std::string> {
  auto format(const lyra::Type& type, std::format_context& ctx) const {
    return std::formatter<std::string>::format(lyra::ToString(type), ctx);
  }
};

template <>
struct std::formatter<lyra::TypeId> : std::formatter<uint32_t> {
  auto format(lyra::TypeId id, std::format_context& ctx) const {
    return std::formatter<uint32_t>::format(id.value, ctx);
  }
};
