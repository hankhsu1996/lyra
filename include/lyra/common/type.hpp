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
  kString,
  kReal,
  kShortReal,
  kPackedArray,
  kPackedStruct,
  kUnpackedArray,
  kUnpackedStruct,
  kDynamicArray,
  kQueue,
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

struct PackedArrayInfo {
  TypeId element_type;
  ConstantRange range;

  auto operator==(const PackedArrayInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const PackedArrayInfo& info) -> H {
    return H::combine(std::move(h), info.element_type, info.range);
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

struct DynamicArrayInfo {
  TypeId element_type;

  auto operator==(const DynamicArrayInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const DynamicArrayInfo& info) -> H {
    return H::combine(std::move(h), info.element_type);
  }
};

struct QueueInfo {
  TypeId element_type;
  uint32_t max_bound;  // 0 = unbounded

  auto operator==(const QueueInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const QueueInfo& info) -> H {
    return H::combine(std::move(h), info.element_type, info.max_bound);
  }
};

struct PackedStructField {
  std::string name;
  TypeId type;
  uint32_t bit_offset;  // From LSB of container (IEEE 1800 packed layout)
  uint32_t bit_width;   // Accounts for nested packed arrays

  auto operator==(const PackedStructField&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const PackedStructField& field) -> H {
    return H::combine(
        std::move(h), field.name, field.type, field.bit_offset,
        field.bit_width);
  }
};

struct PackedStructInfo {
  std::string name;
  std::vector<PackedStructField> fields;
  uint32_t total_bit_width;
  bool is_signed;
  bool is_four_state;

  auto operator==(const PackedStructInfo&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const PackedStructInfo& info) -> H {
    return H::combine(
        std::move(h), info.name, info.fields, info.total_bit_width,
        info.is_signed, info.is_four_state);
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
    std::monostate, IntegralInfo, PackedArrayInfo, PackedStructInfo,
    UnpackedArrayInfo, UnpackedStructInfo, DynamicArrayInfo, QueueInfo>;

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
  static auto PackedArray(TypeId element, ConstantRange range) -> Type;
  static auto PackedStruct(PackedStructInfo info) -> Type;
  static auto UnpackedArray(TypeId element, ConstantRange range) -> Type;
  static auto UnpackedStruct(std::string name, std::vector<StructField> fields)
      -> Type;
  static auto DynamicArray(TypeId element) -> Type;
  static auto Queue(TypeId element, uint32_t max_bound) -> Type;

  [[nodiscard]] auto Kind() const -> TypeKind {
    return kind_;
  }

  // Kind-specific accessors (assert if wrong kind)
  [[nodiscard]] auto AsIntegral() const -> const IntegralInfo&;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayInfo&;
  [[nodiscard]] auto AsPackedStruct() const -> const PackedStructInfo&;
  [[nodiscard]] auto AsUnpackedArray() const -> const UnpackedArrayInfo&;
  [[nodiscard]] auto AsUnpackedStruct() const -> const UnpackedStructInfo&;
  [[nodiscard]] auto AsDynamicArray() const -> const DynamicArrayInfo&;
  [[nodiscard]] auto AsQueue() const -> const QueueInfo&;

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

inline auto Type::PackedArray(TypeId element, ConstantRange range) -> Type {
  Type t;
  t.kind_ = TypeKind::kPackedArray;
  t.payload_ = PackedArrayInfo{.element_type = element, .range = range};
  return t;
}

inline auto Type::PackedStruct(PackedStructInfo info) -> Type {
  Type t;
  t.kind_ = TypeKind::kPackedStruct;
  t.payload_ = std::move(info);
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

inline auto Type::DynamicArray(TypeId element) -> Type {
  Type t;
  t.kind_ = TypeKind::kDynamicArray;
  t.payload_ = DynamicArrayInfo{.element_type = element};
  return t;
}

inline auto Type::Queue(TypeId element, uint32_t max_bound) -> Type {
  Type t;
  t.kind_ = TypeKind::kQueue;
  t.payload_ = QueueInfo{.element_type = element, .max_bound = max_bound};
  return t;
}

inline auto Type::AsIntegral() const -> const IntegralInfo& {
  assert(kind_ == TypeKind::kIntegral);
  return std::get<IntegralInfo>(payload_);
}

inline auto Type::AsPackedArray() const -> const PackedArrayInfo& {
  assert(kind_ == TypeKind::kPackedArray);
  return std::get<PackedArrayInfo>(payload_);
}

inline auto Type::AsPackedStruct() const -> const PackedStructInfo& {
  assert(kind_ == TypeKind::kPackedStruct);
  return std::get<PackedStructInfo>(payload_);
}

inline auto Type::AsUnpackedArray() const -> const UnpackedArrayInfo& {
  assert(kind_ == TypeKind::kUnpackedArray);
  return std::get<UnpackedArrayInfo>(payload_);
}

inline auto Type::AsUnpackedStruct() const -> const UnpackedStructInfo& {
  assert(kind_ == TypeKind::kUnpackedStruct);
  return std::get<UnpackedStructInfo>(payload_);
}

inline auto Type::AsDynamicArray() const -> const DynamicArrayInfo& {
  assert(kind_ == TypeKind::kDynamicArray);
  return std::get<DynamicArrayInfo>(payload_);
}

inline auto Type::AsQueue() const -> const QueueInfo& {
  assert(kind_ == TypeKind::kQueue);
  return std::get<QueueInfo>(payload_);
}

inline auto IsPacked(const Type& type) -> bool {
  return type.Kind() == TypeKind::kIntegral ||
         type.Kind() == TypeKind::kPackedArray ||
         type.Kind() == TypeKind::kPackedStruct;
}

inline auto ToString(TypeKind kind) -> std::string {
  switch (kind) {
    case TypeKind::kVoid:
      return "void";
    case TypeKind::kIntegral:
      return "integral";
    case TypeKind::kString:
      return "string";
    case TypeKind::kReal:
      return "real";
    case TypeKind::kShortReal:
      return "shortreal";
    case TypeKind::kPackedArray:
      return "packed_array";
    case TypeKind::kPackedStruct:
      return "packed_struct";
    case TypeKind::kUnpackedArray:
      return "unpacked_array";
    case TypeKind::kUnpackedStruct:
      return "unpacked_struct";
    case TypeKind::kDynamicArray:
      return "dynamic_array";
    case TypeKind::kQueue:
      return "queue";
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
    case TypeKind::kString:
      return "string";
    case TypeKind::kReal:
      return "real";
    case TypeKind::kShortReal:
      return "shortreal";
    case TypeKind::kPackedArray: {
      const auto& info = type.AsPackedArray();
      return std::format(
          "packed[{}:{}]<type#{}>", info.range.left, info.range.right,
          info.element_type.value);
    }
    case TypeKind::kPackedStruct: {
      const auto& info = type.AsPackedStruct();
      return std::format("packed struct {}", info.name);
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return std::format(
          "type#{}[{}:{}]", info.element_type.value, info.range.left,
          info.range.right);
    }
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      return std::format("struct {}", info.name);
    }
    case TypeKind::kDynamicArray: {
      const auto& info = type.AsDynamicArray();
      return std::format("type#{}[]", info.element_type.value);
    }
    case TypeKind::kQueue: {
      const auto& info = type.AsQueue();
      if (info.max_bound == 0) {
        return std::format("type#{}[$]", info.element_type.value);
      }
      return std::format(
          "type#{}[$:{}]", info.element_type.value, info.max_bound);
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
