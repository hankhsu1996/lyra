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
  kExternalUnitObject,
  kOwningPtr,
  kVector,
  kExternalRef,
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

// LRM 7.2.1 / 7.3.1 packed struct and packed union have no MIR-level
// distinct shape: HIR -> MIR translates `hir::PackedStructType` and
// `hir::PackedUnionType` to their "single vector" projection
// (`PackedArrayType`). Field accesses lower to constant-bounds RangeSelect
// against that vector. A future MIR node for *unpacked* struct / union is
// a genuinely different shape and will get its own variant when that work
// lands.

// MIR tracks unpacked arrays as plain C++ vectors: element type plus a
// non-zero element count. The SV declared range (`[left:right]`, descending
// or with a non-zero base) is resolved at the HIR-to-MIR boundary -- the
// index translation lives inside ElementSelectExpr.index, not on the type.
struct UnpackedArrayType {
  TypeId element_type;
  std::uint64_t size;
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

// The cross-unit twin of ObjectType: the target is another unit's name,
// resolved by name at link time, not a scope of this unit -- so its layout is
// not visible here.
struct ExternalUnitObjectType {
  std::string unit_name;

  auto operator==(const ExternalUnitObjectType&) const -> bool = default;
};

struct OwningPtrType {
  TypeId pointee;

  auto operator==(const OwningPtrType&) const -> bool = default;
};

struct VectorType {
  TypeId element;

  auto operator==(const VectorType&) const -> bool = default;
};

// One by-name step from a resolved scope into an owned child it answers for:
// the child member's name plus one index per array dimension (empty for a
// scalar child). The ancestor indexes its own storage, so a multi-dimensional
// instance array is just more indices, never a flattened offset. (Distinct from
// StructuralHops, which is a count of lexical frames climbed, not a path step.)
struct ChildStep {
  std::string name;
  std::vector<std::uint32_t> indices;

  auto operator==(const ChildStep&) const -> bool = default;
};

// An upward hierarchical reference's storage: a resolved pointer to a leaf of
// `element`, reached by climbing to the ancestor named `ancestor`, walking
// `tail` down through its owned children by name, then fetching `signal` (LRM
// 23.8). `tail` is empty when the leaf sits directly on the ancestor. Like
// ExternalUnitObjectType it carries the cross-unit symbol on the type; the
// address binds at construction, never the layout. Emitted as the runtime
// `ExternUp<element>` member.
struct ExternalRefType {
  TypeId element;
  std::string ancestor;
  std::vector<ChildStep> tail;
  std::string signal;

  auto operator==(const ExternalRefType&) const -> bool = default;
};

using TypeData = std::variant<
    PackedArrayType, EnumType, UnpackedArrayType, DynamicArrayType, QueueType,
    AssociativeArrayType, StringType, EventType, RealType, ShortRealType,
    RealTimeType, ChandleType, VoidType, ObjectType, ExternalUnitObjectType,
    OwningPtrType, VectorType, ExternalRefType>;

struct Type {
  TypeData data;

  [[nodiscard]] auto Kind() const -> TypeKind;
  [[nodiscard]] auto IsPackedArray() const -> bool;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayType&;
  [[nodiscard]] auto IsEnum() const -> bool;
  [[nodiscard]] auto AsEnum() const -> const EnumType&;
  // True for any type whose value-level shape is a single packed vector:
  // PackedArrayType or EnumType (base). Sites that treat the type as its
  // integral representation use this predicate; sites that need to
  // distinguish should match on the variant directly.
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

enum class OwnedChildKind { kModuleInstance, kGenerateScope };

// The classification of an owned-child member, read off the leaf object type
// after stripping any vector layers: an intra-unit object is a generate scope
// (with its target scope), an external-unit object is a module instance.
struct OwnedChildLeaf {
  OwnedChildKind kind = OwnedChildKind::kModuleInstance;
  std::optional<StructuralScopeId> intra_target;
};

[[nodiscard]] auto GetOwnedChildLeaf(const CompilationUnit& unit, TypeId type)
    -> std::optional<OwnedChildLeaf>;

}  // namespace lyra::mir
