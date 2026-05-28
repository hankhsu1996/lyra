#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

enum class TypeKind {
  kPackedArray,
  kEnum,
  kUnpackedArray,
  kDynamicArray,
  kQueue,
  kAssociativeArray,
  kString,
  kEvent,
  kReal,
  kShortReal,
  kRealTime,
  kChandle,
  kVoid,
  kObject,
  kOwningPtr,
  kVector,
};

enum class BitAtom {
  kBit,
  kLogic,
  kReg,
};

enum class Signedness {
  kSigned,
  kUnsigned,
};

enum class PackedArrayForm {
  kExplicit,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kInteger,
  kTime,
};

struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t;
  [[nodiscard]] auto IsAscending() const -> bool;
  [[nodiscard]] auto Contains(std::int64_t index) const -> bool;
  [[nodiscard]] auto LinearOffset(std::int64_t index) const -> std::uint64_t;
};

struct PackedArrayType {
  BitAtom atom;
  Signedness signedness;
  std::vector<PackedRange> dims;
  PackedArrayForm form;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto IsFourState() const -> bool;
};

struct EnumMember {
  std::string name;
  std::int64_t value;
};

struct EnumType {
  PackedArrayType base;
  std::vector<EnumMember> members;
};

struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;
};

struct UnpackedArrayType {
  TypeId element_type;
  std::vector<UnpackedRange> dims;
};

struct DynamicArrayType {
  TypeId element_type;
};

struct QueueType {
  TypeId element_type;
  std::optional<std::uint64_t> max_bound;
};

struct AssociativeArrayType {
  TypeId element_type;
  std::optional<TypeId> key_type;
};

struct StringType {};
struct EventType {};
struct RealType {};
struct ShortRealType {};
struct RealTimeType {};
struct ChandleType {};
struct VoidType {};

struct ObjectType {
  StructuralScopeId target;

  auto operator==(const ObjectType&) const -> bool = default;
};

struct OwningPtrType {
  TypeId pointee;

  auto operator==(const OwningPtrType&) const -> bool = default;
};

struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

using TypeData = std::variant<
    PackedArrayType, EnumType, UnpackedArrayType, DynamicArrayType, QueueType,
    AssociativeArrayType, StringType, EventType, RealType, ShortRealType,
    RealTimeType, ChandleType, VoidType, ObjectType, OwningPtrType, VectorType>;

struct Type {
  TypeData data;

  [[nodiscard]] auto Kind() const -> TypeKind;
  [[nodiscard]] auto IsPackedArray() const -> bool;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayType&;
  [[nodiscard]] auto IsEnum() const -> bool;
  [[nodiscard]] auto AsEnum() const -> const EnumType&;
  // True for PackedArrayType or EnumType (since enum's value-level shape is
  // its base PackedArray). Sites that treat enum as its integral
  // representation use this predicate; sites that need to distinguish enum
  // from packed should match on the variant directly.
  [[nodiscard]] auto IsIntegralPacked() const -> bool;
  [[nodiscard]] auto AsIntegralPacked() const -> const PackedArrayType&;
};

class CompilationUnit;

[[nodiscard]] auto IsOwningObjectType(const CompilationUnit& unit, TypeId type)
    -> bool;

[[nodiscard]] auto IsVectorOfOwningObjectType(
    const CompilationUnit& unit, TypeId type) -> bool;

[[nodiscard]] auto GetOwnedObjectTarget(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<StructuralScopeId>;

}  // namespace lyra::mir
