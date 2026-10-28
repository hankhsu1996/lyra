#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

auto PackedRange::ElementCount() const -> std::uint64_t {
  const std::int64_t span = (left >= right) ? (left - right) : (right - left);
  return static_cast<std::uint64_t>(span) + 1U;
}

auto PackedRange::IsAscending() const -> bool {
  return left <= right;
}

auto PackedRange::Contains(std::int64_t index) const -> bool {
  const std::int64_t lo = std::min(left, right);
  const std::int64_t hi = std::max(left, right);
  return index >= lo && index <= hi;
}

// A packed array's elements read most significant first from the dimension's
// left end (LRM 7.4.1), so the element the dimension names first is offset
// zero however the range was declared -- which is the order a concatenation of
// those elements takes them in, and the reverse of index order whenever the
// dimension descends.
auto PackedRange::LinearOffset(std::int64_t index) const -> std::uint64_t {
  if (!Contains(index)) {
    throw InternalError("PackedRange::LinearOffset: index out of range");
  }
  if (left >= right) {
    return static_cast<std::uint64_t>(left - index);
  }
  return static_cast<std::uint64_t>(index - left);
}

auto UnpackedRange::ElementCount() const -> std::uint64_t {
  const std::int64_t span = (left >= right) ? (left - right) : (right - left);
  return static_cast<std::uint64_t>(span) + 1U;
}

auto UnpackedRange::LowestIndex() const -> std::int64_t {
  return std::min(left, right);
}

namespace {

template <typename T>
auto Combine(std::size_t seed, const T& value) -> std::size_t {
  base::HashField(seed, value);
  return seed;
}

auto HashTypeId(std::size_t seed, TypeId id) -> std::size_t {
  return Combine(seed, id.value);
}

// The identities a field list names. Field names are left to equality: two
// aggregates whose members have the same types in the same order are rare
// enough that separating them by name buys nothing.
template <typename Field>
auto HashFields(std::size_t seed, const std::vector<Field>& fields)
    -> std::size_t {
  std::size_t hash = Combine(seed, fields.size());
  for (const Field& field : fields) {
    hash = HashTypeId(hash, field.type);
  }
  return hash;
}

// A declared type is the declaration it names, so that is all it hashes by.
auto HashDeclaration(std::size_t seed, const TypeDeclarationRef& declaration)
    -> std::size_t {
  return Combine(Combine(seed, declaration.unit_name), declaration.path);
}

}  // namespace

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  const std::size_t seed = Combine(std::size_t{0}, type.data_.index());
  return type.Visit(
      Overloaded{
          [seed](const ScalarBitType& t) {
            return Combine(seed, static_cast<std::size_t>(t.atom));
          },
          [seed](const PackedArrayType& t) {
            return Combine(
                Combine(HashTypeId(seed, t.element_type), t.dim.left),
                t.dim.right);
          },
          [seed](const PackedStructType& t) {
            return HashFields(seed, t.fields);
          },
          [seed](const PackedUnionType& t) {
            return HashFields(seed, t.fields);
          },
          [seed](const EnumType& t) {
            return HashDeclaration(seed, t.declaration);
          },
          [seed](const UnpackedStructType& t) {
            return HashDeclaration(seed, t.declaration);
          },
          [seed](const UnpackedUnionType& t) {
            return HashDeclaration(seed, t.declaration);
          },
          [seed](const UnpackedArrayType& t) {
            return Combine(
                Combine(HashTypeId(seed, t.element_type), t.dim.left),
                t.dim.right);
          },
          [seed](const DynamicArrayType& t) {
            return HashTypeId(seed, t.element_type);
          },
          [seed](const QueueType& t) {
            return Combine(
                HashTypeId(seed, t.element_type), t.max_bound.value_or(0));
          },
          [seed](const AssociativeArrayType& t) {
            return HashTypeId(HashTypeId(seed, t.element_type), t.key_type);
          },
          [seed](const ClassHandleType& t) {
            return Combine(seed, t.class_id.value);
          },
          [seed](const ImportedClassHandleType& t) {
            return Combine(seed, static_cast<std::size_t>(t.klass));
          },
          [seed](const WeakHandleType& t) {
            return Combine(seed, t.handle.value);
          },
          [seed](const CovergroupHandleType& t) {
            return Combine(seed, t.covergroup.value);
          },
          [seed](const UnitObjectType& t) {
            return Combine(seed, t.scope_class.value);
          },
          [seed](const UnitObjectsType& t) {
            std::size_t h = seed;
            for (const UnpackedRange& range : t.ranges) {
              h = Combine(Combine(h, range.left), range.right);
            }
            for (const UnitObjectType& kind : t.kinds) {
              h = Combine(h, kind.scope_class.value);
            }
            for (const std::uint32_t taken : t.taken) {
              h = Combine(h, static_cast<std::size_t>(taken));
            }
            return h;
          },
          [seed](const VirtualInterfaceType& t) {
            return Combine(seed, t.unit_name);
          },
          // A type carrying nothing beyond being itself is separated by its arm
          // alone, which the seed already holds.
          [seed](const WildcardIndexType&) { return seed; },
          [seed](const StringType&) { return seed; },
          [seed](const EventType&) { return seed; },
          [seed](const RealType&) { return seed; },
          [seed](const ShortRealType&) { return seed; },
          [seed](const RealTimeType&) { return seed; },
          [seed](const ChandleType&) { return seed; },
          [seed](const NullType&) { return seed; },
          [seed](const DynamicType&) { return seed; },
          [seed](const VoidType&) { return seed; }});
}

auto Type::IsIntegral() const -> bool {
  // One arm per HIR type and no catch-all, so a type added later fails to
  // compile here until it says which side it is on.
  return Visit(
      Overloaded{
          [](const ScalarBitType&) { return true; },
          [](const PackedArrayType&) { return true; },
          [](const PackedStructType&) { return true; },
          [](const PackedUnionType&) { return true; },
          [](const EnumType&) { return true; },
          [](const UnpackedStructType&) { return false; },
          [](const UnpackedUnionType&) { return false; },
          [](const UnpackedArrayType&) { return false; },
          [](const DynamicArrayType&) { return false; },
          [](const QueueType&) { return false; },
          [](const AssociativeArrayType&) { return false; },
          [](const WildcardIndexType&) { return false; },
          [](const StringType&) { return false; },
          [](const EventType&) { return false; },
          [](const RealType&) { return false; },
          [](const ShortRealType&) { return false; },
          [](const RealTimeType&) { return false; },
          [](const ChandleType&) { return false; },
          [](const ClassHandleType&) { return false; },
          [](const ImportedClassHandleType&) { return false; },
          [](const WeakHandleType&) { return false; },
          [](const CovergroupHandleType&) { return false; },
          [](const UnitObjectType&) { return false; },
          [](const UnitObjectsType&) { return false; },
          [](const VirtualInterfaceType&) { return false; },
          [](const NullType&) { return false; },
          [](const DynamicType&) { return false; },
          [](const VoidType&) { return false; },
      });
}

auto Type::IsValueChangeObservable() const -> bool {
  // One arm per HIR type and no catch-all, so a type added later fails to
  // compile here until it says which side it is on.
  return Visit(
      Overloaded{
          [](const ScalarBitType&) { return true; },
          [](const PackedArrayType&) { return true; },
          [](const PackedStructType&) { return true; },
          [](const PackedUnionType&) { return true; },
          [](const EnumType&) { return true; },
          [](const UnpackedStructType&) { return true; },
          [](const UnpackedUnionType&) { return true; },
          [](const UnpackedArrayType&) { return true; },
          [](const DynamicArrayType&) { return true; },
          [](const QueueType&) { return true; },
          [](const AssociativeArrayType&) { return true; },
          [](const StringType&) { return true; },
          [](const RealType&) { return true; },
          [](const ShortRealType&) { return true; },
          [](const RealTimeType&) { return true; },
          // A handle's value is which object it names, and LRM 9.4.2 makes a
          // write to such a variable, or to a chandle, an event whenever what
          // it holds is not what it held before. It separates that from a wait
          // on a property of the object, which the object answers.
          [](const ClassHandleType&) { return true; },
          [](const ImportedClassHandleType&) { return true; },
          // What a weak handle refers to changes with no write to it, when the
          // object expires, so a wait sees that through what reading it
          // answers, never as a change of the handle (LRM 8.30.3).
          [](const WeakHandleType&) { return false; },
          [](const CovergroupHandleType&) { return true; },
          [](const ChandleType&) { return true; },
          // Which instance a virtual interface holds is a value it is compared
          // by (LRM 25.9), so a wait on it waits for a different instance.
          [](const VirtualInterfaceType&) { return true; },
          // Neither is a type a declaration names: one only indexes an array
          // (LRM 7.8.1), and the other only a mailbox's own messages are of
          // (LRM 15.4.9), which nothing but the mailbox reads.
          [](const WildcardIndexType&) { return false; },
          [](const DynamicType&) { return false; },
          [](const EventType&) { return false; },
          [](const UnitObjectType&) { return false; },
          [](const UnitObjectsType&) { return false; },
          [](const NullType&) { return false; },
          [](const VoidType&) { return false; },
      });
}

auto Type::ContainerElementType() const -> std::optional<TypeId> {
  using Element = std::optional<TypeId>;
  // One arm per HIR type and no catch-all, so a type added later fails to
  // compile here until it says whether it holds a sequence of values.
  return Visit(
      Overloaded{
          // The four a declaration names as holding a sequence of values,
          // however the sequence is sized and however it is indexed.
          [](const UnpackedArrayType& t) -> Element { return t.element_type; },
          [](const DynamicArrayType& t) -> Element { return t.element_type; },
          [](const QueueType& t) -> Element { return t.element_type; },
          [](const AssociativeArrayType& t) -> Element {
            return t.element_type;
          },

          // A packed array names an element type and a packed aggregate names
          // members, and neither is a sequence of values: the whole of such a
          // type is one vector of bits, so reaching into it selects bits of
          // that vector rather than reading a value held beside the others.
          [](const PackedArrayType&) -> Element { return std::nullopt; },
          [](const PackedStructType&) -> Element { return std::nullopt; },
          [](const PackedUnionType&) -> Element { return std::nullopt; },
          [](const EnumType&) -> Element { return std::nullopt; },

          // An unpacked aggregate holds its members at once rather than a run
          // of one type, so there is no single element type to answer with.
          [](const UnpackedStructType&) -> Element { return std::nullopt; },
          [](const UnpackedUnionType&) -> Element { return std::nullopt; },

          // A single quantity, a single token, a handle to something living
          // elsewhere, or nothing at all.
          [](const ScalarBitType&) -> Element { return std::nullopt; },
          [](const StringType&) -> Element { return std::nullopt; },
          [](const RealType&) -> Element { return std::nullopt; },
          [](const ShortRealType&) -> Element { return std::nullopt; },
          [](const RealTimeType&) -> Element { return std::nullopt; },
          [](const WildcardIndexType&) -> Element { return std::nullopt; },
          [](const EventType&) -> Element { return std::nullopt; },
          [](const ChandleType&) -> Element { return std::nullopt; },
          [](const ClassHandleType&) -> Element { return std::nullopt; },
          [](const ImportedClassHandleType&) -> Element {
            return std::nullopt;
          },
          [](const WeakHandleType&) -> Element { return std::nullopt; },
          [](const CovergroupHandleType&) -> Element { return std::nullopt; },
          [](const UnitObjectType&) -> Element { return std::nullopt; },
          [](const UnitObjectsType&) -> Element { return std::nullopt; },
          [](const VirtualInterfaceType&) -> Element { return std::nullopt; },
          [](const NullType&) -> Element { return std::nullopt; },
          [](const DynamicType&) -> Element { return std::nullopt; },
          [](const VoidType&) -> Element { return std::nullopt; },
      });
}

auto Type::NamedDeclaration() const -> const TypeDeclarationRef* {
  using Declared = const TypeDeclarationRef*;
  // One arm per HIR type and no catch-all, so a type added later fails to
  // compile here until it says whether it is a declaration.
  return Visit(
      Overloaded{
          // A class is named by a declaration as well, and its definition is
          // held apart by the class registry rather than beside these.
          [](const EnumType& t) -> Declared { return &t.declaration; },
          [](const UnpackedStructType& t) -> Declared {
            return &t.declaration;
          },
          [](const UnpackedUnionType& t) -> Declared { return &t.declaration; },

          [](const ScalarBitType&) -> Declared { return nullptr; },
          [](const PackedArrayType&) -> Declared { return nullptr; },
          [](const PackedStructType&) -> Declared { return nullptr; },
          [](const PackedUnionType&) -> Declared { return nullptr; },
          [](const UnpackedArrayType&) -> Declared { return nullptr; },
          [](const DynamicArrayType&) -> Declared { return nullptr; },
          [](const QueueType&) -> Declared { return nullptr; },
          [](const AssociativeArrayType&) -> Declared { return nullptr; },
          [](const WildcardIndexType&) -> Declared { return nullptr; },
          [](const StringType&) -> Declared { return nullptr; },
          [](const EventType&) -> Declared { return nullptr; },
          [](const RealType&) -> Declared { return nullptr; },
          [](const ShortRealType&) -> Declared { return nullptr; },
          [](const RealTimeType&) -> Declared { return nullptr; },
          [](const ChandleType&) -> Declared { return nullptr; },
          [](const ClassHandleType&) -> Declared { return nullptr; },
          [](const ImportedClassHandleType&) -> Declared { return nullptr; },
          [](const WeakHandleType&) -> Declared { return nullptr; },
          [](const CovergroupHandleType&) -> Declared { return nullptr; },
          [](const UnitObjectType&) -> Declared { return nullptr; },
          [](const UnitObjectsType&) -> Declared { return nullptr; },
          [](const VirtualInterfaceType&) -> Declared { return nullptr; },
          [](const NullType&) -> Declared { return nullptr; },
          [](const DynamicType&) -> Declared { return nullptr; },
          [](const VoidType&) -> Declared { return nullptr; },
      });
}

}  // namespace lyra::hir
