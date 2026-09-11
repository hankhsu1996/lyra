#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <type_traits>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::mir {

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

auto PackedArrayType::BitWidth() const -> std::uint64_t {
  if (dims.empty()) {
    return 1U;
  }
  std::uint64_t width = 1U;
  for (const auto& dim : dims) {
    width *= dim.ElementCount();
  }
  return width;
}

auto BitsOf(MachineIntWidth width) -> std::uint32_t {
  switch (width) {
    case MachineIntWidth::k8:
      return 8;
    case MachineIntWidth::k16:
      return 16;
    case MachineIntWidth::k32:
      return 32;
    case MachineIntWidth::k64:
      return 64;
  }
  throw InternalError("mir: unknown MachineIntWidth");
}

auto BitsOf(MachineFloatWidth width) -> std::uint32_t {
  switch (width) {
    case MachineFloatWidth::k32:
      return 32;
    case MachineFloatWidth::k64:
      return 64;
  }
  throw InternalError("mir: unknown MachineFloatWidth");
}

namespace {

void HashCombine(std::size_t& seed, std::size_t value) {
  seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

template <typename T>
void HashField(std::size_t& seed, const T& value) {
  HashCombine(seed, std::hash<T>{}(value));
}

void HashId(std::size_t& seed, TypeId id) {
  HashCombine(seed, std::hash<std::uint32_t>{}(id.value));
}

void HashIds(std::size_t& seed, const std::vector<TypeId>& ids) {
  for (const TypeId id : ids) {
    HashId(seed, id);
  }
}

template <typename E>
  requires std::is_enum_v<E>
void HashEnum(std::size_t& seed, E value) {
  HashField(seed, static_cast<std::uint64_t>(value));
}

// Hashes a packed array by the three attributes that decide what it means --
// four-stateness, signedness, and the dimensions its width comes from.
void HashPackedShape(std::size_t& seed, const PackedArrayType& packed) {
  HashEnum(seed, packed.state_kind);
  HashEnum(seed, packed.signedness);
  for (const PackedRange& dim : packed.dims) {
    HashField(seed, dim.left);
    HashField(seed, dim.right);
  }
}

void HashMembers(
    std::size_t& seed, const std::vector<AggregateMember>& members) {
  for (const AggregateMember& member : members) {
    HashField(seed, member.name);
    HashId(seed, member.type);
  }
}

}  // namespace

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit(
      Overloaded{
          [&](const PackedArrayType& t) { HashPackedShape(seed, t); },
          [&](const EnumType& t) {
            HashPackedShape(seed, t.base);
            for (const EnumMember& member : t.members) {
              HashField(seed, member.name);
              HashField(seed, member.value);
            }
          },
          [&](const PackedStructType& t) {
            HashPackedShape(seed, t.base);
            HashMembers(seed, t.members);
          },
          [&](const PackedUnionType& t) {
            HashPackedShape(seed, t.base);
            HashMembers(seed, t.members);
          },
          [&](const UnpackedArrayType& t) {
            HashId(seed, t.element_type);
            HashField(seed, t.dim.left);
            HashField(seed, t.dim.right);
          },
          [&](const DynamicArrayType& t) { HashId(seed, t.element_type); },
          [&](const QueueType& t) {
            HashId(seed, t.element_type);
            HashField(seed, t.max_bound.value_or(0));
            HashField(seed, t.max_bound.has_value());
          },
          [&](const AssociativeArrayType& t) {
            HashId(seed, t.element_type);
            HashId(seed, t.key_type);
          },
          [](const WildcardIndexType&) {},
          [](const StringType&) {},
          [](const MachineCStringType&) {},
          [](const MachineBoolType&) {},
          [&](const MachineIntType& t) {
            HashEnum(seed, t.width);
            HashEnum(seed, t.signedness);
          },
          [&](const MachineFloatType& t) { HashEnum(seed, t.width); },
          [&](const MachineArrayType& t) {
            HashId(seed, t.element);
            HashField(seed, t.size);
          },
          [&](const MachineFunctionType& t) {
            HashIds(seed, t.params);
            HashId(seed, t.result);
          },
          [](const EventType&) {},
          [](const RealType&) {},
          [](const ShortRealType&) {},
          [](const RealTimeType&) {},
          [](const ChandleType&) {},
          [](const VoidType&) {},
          [&](const ObjectType& t) { HashField(seed, t.class_id.value); },
          [&](const ExternalUnitObjectType& t) {
            HashField(seed, t.object.value);
          },
          [&](const CrossUnitClassType& t) {
            HashField(seed, t.unit_name);
            HashField(seed, t.class_name);
          },
          [](const OpaqueObjectType&) {},
          [&](const RuntimeClassType& t) { HashField(seed, t.symbol); },
          [](const RuntimeEffectsType&) {},
          [](const FilesType&) {},
          [](const DiagnosticType&) {},
          [&](const RuntimeLibraryType& t) { HashEnum(seed, t.kind); },
          [&](const CoroutineType& t) { HashId(seed, t.payload); },
          [&](const RefType& t) {
            HashId(seed, t.pointee);
            HashEnum(seed, t.mutability);
          },
          [&](const PointerType& t) {
            HashId(seed, t.pointee);
            HashEnum(seed, t.ownership);
            HashEnum(seed, t.mutability);
          },
          [&](const ManagedRefType& t) { HashId(seed, t.pointee); },
          [&](const VectorType& t) { HashId(seed, t.element); },
          [&](const TupleType& t) { HashIds(seed, t.elements); },
          [&](const UnpackedStructType& t) { HashMembers(seed, t.members); },
          [&](const UnionType& t) { HashMembers(seed, t.members); },
          [&](const TaggedUnionType& t) { HashMembers(seed, t.members); },
          [](const EmptyType&) {},
          [&](const ObservableType& t) { HashId(seed, t.value); },
          [&](const ResolvedType& t) { HashId(seed, t.value); },
          [&](const DriverType& t) { HashId(seed, t.value); },
          [&](const SampledHistoryType& t) { HashId(seed, t.value); },
          [](const EvaluationAttemptsType&) {},
          [&](const StructType& t) { HashField(seed, t.struct_id.value); },
          [&](const ClosureType& t) { HashField(seed, t.closure_id.value); }});
  return seed;
}

auto MemberTypes(const std::vector<AggregateMember>& members)
    -> std::vector<TypeId> {
  std::vector<TypeId> types;
  types.reserve(members.size());
  for (const AggregateMember& member : members) {
    types.push_back(member.type);
  }
  return types;
}

auto Type::IsIntegralPacked() const -> bool {
  return Is<PackedArrayType>() || Is<EnumType>() || Is<PackedStructType>() ||
         Is<PackedUnionType>();
}

auto Type::PackedShape() const -> const PackedArrayType& {
  if (const auto* packed = As<PackedArrayType>()) {
    return *packed;
  }
  if (const auto* enumeration = As<EnumType>()) {
    return enumeration->base;
  }
  if (const auto* packed_struct = As<PackedStructType>()) {
    return packed_struct->base;
  }
  if (const auto* packed_union = As<PackedUnionType>()) {
    return packed_union->base;
  }
  throw InternalError("mir: type has no packed shape; it is not integral");
}

auto Type::IsProduct() const -> bool {
  return Is<TupleType>() || Is<UnpackedStructType>();
}

auto Type::ProductComponentTypes() const -> std::vector<TypeId> {
  if (const auto* tuple = As<TupleType>()) {
    return tuple->elements;
  }
  if (const auto* structure = As<UnpackedStructType>()) {
    return MemberTypes(structure->members);
  }
  throw InternalError("mir: type is not a product");
}

auto Type::IsRealFamily() const -> bool {
  return Is<RealType>() || Is<ShortRealType>() || Is<RealTimeType>();
}

auto Type::IsAliasHandle() const -> bool {
  return Is<RuntimeEffectsType>() || Is<FilesType>() || Is<DiagnosticType>();
}

auto Type::IsCapabilityWrapper() const -> bool {
  return Is<ObservableType>() || Is<RefType>() || Is<ResolvedType>() ||
         Is<DriverType>();
}

auto Type::WrappedValueType() const -> TypeId {
  if (const auto* observable = As<ObservableType>()) {
    return observable->value;
  }
  if (const auto* reference = As<RefType>()) {
    return reference->pointee;
  }
  if (const auto* resolved = As<ResolvedType>()) {
    return resolved->value;
  }
  if (const auto* driver = As<DriverType>()) {
    return driver->value;
  }
  throw InternalError("mir: type is not a capability wrapper");
}

auto Type::HeldValueTypes() const -> std::vector<TypeId> {
  using Held = std::vector<TypeId>;
  return Visit(
      Overloaded{
          // An integral value is one vector of bits, and an enumeration or a
          // packed aggregate is that vector under a set of names. All are
          // indivisible, as is every other value that is a single quantity, a
          // single token, or nothing: a packed member is a run of the one
          // vector rather than a value held beside it.
          [](const PackedArrayType&) -> Held { return {}; },
          [](const EnumType&) -> Held { return {}; },
          [](const PackedStructType&) -> Held { return {}; },
          [](const PackedUnionType&) -> Held { return {}; },
          [](const WildcardIndexType&) -> Held { return {}; },
          [](const StringType&) -> Held { return {}; },
          [](const MachineCStringType&) -> Held { return {}; },
          [](const MachineBoolType&) -> Held { return {}; },
          [](const MachineIntType&) -> Held { return {}; },
          [](const MachineFloatType&) -> Held { return {}; },
          [](const RealType&) -> Held { return {}; },
          [](const ShortRealType&) -> Held { return {}; },
          [](const RealTimeType&) -> Held { return {}; },
          [](const ChandleType&) -> Held { return {}; },
          [](const EventType&) -> Held { return {}; },
          [](const EmptyType&) -> Held { return {}; },
          [](const VoidType&) -> Held { return {}; },

          // A container holds its elements, and a keyed one holds its keys
          // beside them.
          [](const UnpackedArrayType& t) -> Held { return {t.element_type}; },
          [](const DynamicArrayType& t) -> Held { return {t.element_type}; },
          [](const QueueType& t) -> Held { return {t.element_type}; },
          [](const AssociativeArrayType& t) -> Held {
            return {t.key_type, t.element_type};
          },
          [](const MachineArrayType& t) -> Held { return {t.element}; },
          [](const VectorType& t) -> Held { return {t.element}; },

          // A product holds every component at once; a union and a tagged sum
          // hold one at a time, which is still one of these.
          [](const TupleType& t) -> Held { return t.elements; },
          [](const UnpackedStructType& t) -> Held {
            return MemberTypes(t.members);
          },
          [](const UnionType& t) -> Held { return MemberTypes(t.members); },
          [](const TaggedUnionType& t) -> Held {
            return MemberTypes(t.members);
          },

          // A cell holds the value it keeps, however it publishes a change to
          // it and however far back it remembers.
          [](const ObservableType& t) -> Held { return {t.value}; },
          [](const ResolvedType& t) -> Held { return {t.value}; },
          [](const DriverType& t) -> Held { return {t.value}; },
          [](const SampledHistoryType& t) -> Held { return {t.value}; },

          // What a concurrent assertion has in flight is member storage like
          // those, and holds no value of the design at all: the words in it
          // mean something only to the transition that reads them.
          [](const EvaluationAttemptsType&) -> Held { return {}; },

          // These refer to a value living elsewhere rather than holding one:
          // copying the referring value copies no part of what it reaches. A
          // coroutine's payload is what awaiting it produces, not something it
          // carries, and a code address names a signature it is not made of.
          [](const RefType&) -> Held { return {}; },
          [](const PointerType&) -> Held { return {}; },
          [](const ManagedRefType&) -> Held { return {}; },
          [](const CoroutineType&) -> Held { return {}; },
          [](const MachineFunctionType&) -> Held { return {}; },

          // A nominal type names a declaration, and the declaration is what
          // lists the members; the type states none, so a walk that must reach
          // them goes to the registry the id resolves in.
          [](const ObjectType&) -> Held { return {}; },
          [](const ExternalUnitObjectType&) -> Held { return {}; },
          [](const CrossUnitClassType&) -> Held { return {}; },
          [](const OpaqueObjectType&) -> Held { return {}; },
          [](const RuntimeClassType&) -> Held { return {}; },
          [](const StructType&) -> Held { return {}; },
          [](const ClosureType&) -> Held { return {}; },

          // A handle to a runtime facility, and an inert payload the library
          // owns the shape of. Neither is composed of values this layer names.
          [](const RuntimeEffectsType&) -> Held { return {}; },
          [](const FilesType&) -> Held { return {}; },
          [](const DiagnosticType&) -> Held { return {}; },
          [](const RuntimeLibraryType&) -> Held { return {}; }});
}

auto Type::ContainerElementType() const -> std::optional<TypeId> {
  using Element = std::optional<TypeId>;
  return Visit(
      Overloaded{
          // The four a declaration names as holding a run of values, however
          // the run is sized and however it is indexed.
          [](const UnpackedArrayType& t) -> Element { return t.element_type; },
          [](const DynamicArrayType& t) -> Element { return t.element_type; },
          [](const QueueType& t) -> Element { return t.element_type; },
          [](const AssociativeArrayType& t) -> Element {
            return t.element_type;
          },

          // These hold a run of values too and are still not containers: a
          // lowering builds them to carry something, where a container is a
          // type a declaration named.
          [](const MachineArrayType&) -> Element { return std::nullopt; },
          [](const VectorType&) -> Element { return std::nullopt; },

          // One vector of bits, under a set of names or not: what looks like
          // an element is a run of that vector rather than a value held beside
          // the others.
          [](const PackedArrayType&) -> Element { return std::nullopt; },
          [](const EnumType&) -> Element { return std::nullopt; },
          [](const PackedStructType&) -> Element { return std::nullopt; },
          [](const PackedUnionType&) -> Element { return std::nullopt; },

          // Held all at once, or one at a time, but never as a run of one
          // type.
          [](const TupleType&) -> Element { return std::nullopt; },
          [](const UnpackedStructType&) -> Element { return std::nullopt; },
          [](const UnionType&) -> Element { return std::nullopt; },
          [](const TaggedUnionType&) -> Element { return std::nullopt; },

          // A single quantity or a single token.
          [](const WildcardIndexType&) -> Element { return std::nullopt; },
          [](const StringType&) -> Element { return std::nullopt; },
          [](const MachineCStringType&) -> Element { return std::nullopt; },
          [](const MachineBoolType&) -> Element { return std::nullopt; },
          [](const MachineIntType&) -> Element { return std::nullopt; },
          [](const MachineFloatType&) -> Element { return std::nullopt; },
          [](const RealType&) -> Element { return std::nullopt; },
          [](const ShortRealType&) -> Element { return std::nullopt; },
          [](const RealTimeType&) -> Element { return std::nullopt; },
          [](const ChandleType&) -> Element { return std::nullopt; },
          [](const EventType&) -> Element { return std::nullopt; },
          [](const EmptyType&) -> Element { return std::nullopt; },
          [](const VoidType&) -> Element { return std::nullopt; },

          // A cell keeps one value, whatever it publishes about changes to it;
          // the run, where there is one, belongs to the value it keeps.
          [](const ObservableType&) -> Element { return std::nullopt; },
          [](const ResolvedType&) -> Element { return std::nullopt; },
          [](const DriverType&) -> Element { return std::nullopt; },
          [](const SampledHistoryType&) -> Element { return std::nullopt; },
          [](const EvaluationAttemptsType&) -> Element { return std::nullopt; },

          // These refer to a value living elsewhere rather than holding one,
          // so a container reached through one is reached by dereferencing it
          // first.
          [](const RefType&) -> Element { return std::nullopt; },
          [](const PointerType&) -> Element { return std::nullopt; },
          [](const ManagedRefType&) -> Element { return std::nullopt; },
          [](const CoroutineType&) -> Element { return std::nullopt; },
          [](const MachineFunctionType&) -> Element { return std::nullopt; },

          // A nominal type names a declaration, and a declaration is not a run
          // of anything.
          [](const ObjectType&) -> Element { return std::nullopt; },
          [](const ExternalUnitObjectType&) -> Element { return std::nullopt; },
          [](const CrossUnitClassType&) -> Element { return std::nullopt; },
          [](const OpaqueObjectType&) -> Element { return std::nullopt; },
          [](const RuntimeClassType&) -> Element { return std::nullopt; },
          [](const StructType&) -> Element { return std::nullopt; },
          [](const ClosureType&) -> Element { return std::nullopt; },

          // A handle to a runtime facility, and an inert payload the library
          // owns the shape of.
          [](const RuntimeEffectsType&) -> Element { return std::nullopt; },
          [](const FilesType&) -> Element { return std::nullopt; },
          [](const DiagnosticType&) -> Element { return std::nullopt; },
          [](const RuntimeLibraryType&) -> Element { return std::nullopt; }});
}

}  // namespace lyra::mir
