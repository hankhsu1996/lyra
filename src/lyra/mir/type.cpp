#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <type_traits>
#include <vector>

#include "lyra/base/hash.hpp"
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

using base::HashField;

void HashId(std::size_t& seed, TypeId id) {
  HashField(seed, id.value);
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

}  // namespace

void HashPackedShape(std::size_t& seed, const PackedArrayType& packed) {
  HashEnum(seed, packed.state_kind);
  HashEnum(seed, packed.signedness);
  for (const PackedRange& dim : packed.dims) {
    HashField(seed, dim.left);
    HashField(seed, dim.right);
  }
}

void HashEnumeration(std::size_t& seed, const EnumType& enumeration) {
  HashPackedShape(seed, enumeration.base);
  for (const EnumMember& member : enumeration.members) {
    HashField(seed, member.name);
    HashIntegralConstant(seed, member.value);
  }
}

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit(
      Overloaded{
          [&](const PackedArrayType& t) { HashPackedShape(seed, t); },
          [&](const EnumType& t) { HashEnumeration(seed, t); },
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
          [](const ChandleType&) {},
          [](const VoidType&) {},
          [&](const ObjectType& t) {
            std::visit(
                Overloaded{
                    [&](const IntraUnitClassRef& intra) {
                      HashField(seed, intra.class_id.value);
                    },
                    [&](const CrossUnitClassRef& cross) {
                      HashField(seed, cross.unit_name);
                      HashField(seed, cross.class_name);
                    }},
                t.of);
          },
          [&](const RuntimeClassType& t) { HashEnum(seed, t.which); },
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
          [&](const UnionType& t) { HashIds(seed, t.members); },
          [&](const TaggedUnionType& t) { HashIds(seed, t.members); },
          [](const EmptyType&) {},
          [&](const ObservableType& t) { HashId(seed, t.value); },
          [&](const ResolvedType& t) { HashId(seed, t.value); },
          [&](const DriverType& t) { HashId(seed, t.value); },
          [&](const OpenWriteType& t) { HashId(seed, t.value); },
          [&](const DesignationType& t) { HashId(seed, t.value); },
          [&](const ObjectWriteType& t) { HashId(seed, t.object); },
          [&](const SampledHistoryType& t) { HashId(seed, t.value); },
          [](const EvaluationAttemptsType&) {},
          [&](const StructType& t) {
            std::visit(
                Overloaded{
                    [&](StructId id) { HashField(seed, id.value); },
                    [&](const TypeDeclarationRef& ref) {
                      HashField(seed, ref.unit_name);
                      HashField(seed, ref.name);
                    }},
                t.declaration);
          },
          [&](const ClosureType& t) { HashField(seed, t.closure_id.value); }});
  return seed;
}

auto Type::IsIntegralPacked() const -> bool {
  return Is<PackedArrayType>() || Is<EnumType>();
}

auto Type::PackedShape() const -> const PackedArrayType& {
  if (const auto* packed = As<PackedArrayType>()) {
    return *packed;
  }
  if (const auto* enumeration = As<EnumType>()) {
    return enumeration->base;
  }
  throw InternalError("mir: type has no packed shape; it is not integral");
}

auto Type::IsRealFamily() const -> bool {
  return Is<RealType>() || Is<ShortRealType>();
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

auto Type::IsRuntimeStoredValue() const -> bool {
  // One arm per MIR type and no catch-all, so a type added later fails to
  // compile here until it says which side it is on -- rather than silently
  // answering that the holder has the whole of it, which is a wrong answer and
  // not a refusal: the holder goes on holding a handle after the storage behind
  // it is gone.
  return Visit(
      Overloaded{
          // The simulation values, every one of which the runtime builds and
          // keeps: one vector of bits however it is named, a run of values
          // however it is sized and indexed, the ones held all at once or one
          // at a time, and the single quantities whose representation the
          // library still decides.
          [](const PackedArrayType&) { return true; },
          [](const EnumType&) { return true; },
          [](const UnpackedArrayType&) { return true; },
          [](const DynamicArrayType&) { return true; },
          [](const QueueType&) { return true; },
          [](const AssociativeArrayType&) { return true; },
          [](const TupleType&) { return true; },
          [](const StructType&) { return true; },
          [](const UnionType&) { return true; },
          [](const TaggedUnionType&) { return true; },
          [](const StringType&) { return true; },
          [](const RealType&) { return true; },
          [](const ShortRealType&) { return true; },

          // A class handle is one of these and a chandle is not, which LRM 8.4
          // Table 8-1 states directly: an unreferenced object is collected
          // where the handle is an object handle, and is not where it is a
          // chandle or a C pointer. A handle's value is therefore which object
          // it names together with a claim on that object's life, which no
          // address carries on its own; a chandle's value is the address, so
          // whoever holds the address holds the whole of it (LRM 6.14).
          [](const ManagedRefType&) { return true; },
          [](const ChandleType&) { return false; },

          // A machine quantity, which whoever holds it holds entire.
          [](const MachineCStringType&) { return false; },
          [](const MachineBoolType&) { return false; },
          [](const MachineIntType&) { return false; },
          [](const MachineFloatType&) { return false; },
          [](const MachineArrayType&) { return false; },
          [](const MachineFunctionType&) { return false; },

          // An address whose value is the address: what it names lives for its
          // own reasons and ends for them, so nothing about holding the address
          // keeps it or loses it.
          [](const RefType&) { return false; },
          [](const PointerType&) { return false; },
          [](const VectorType&) { return false; },
          [](const DriverType&) { return false; },
          [](const CoroutineType&) { return false; },

          // An object and a closure are reached by their address for the same
          // reason.
          [](const ObjectType&) { return false; },
          [](const RuntimeClassType&) { return false; },
          [](const ClosureType&) { return false; },

          // Storage, and the facilities the runtime holds for the whole run.
          // Each is consumed where it lives rather than read out as a value, so
          // no holder ever has a copy of one to lose.
          [](const ObservableType&) { return false; },
          [](const ResolvedType&) { return false; },
          [](const OpenWriteType&) { return false; },
          [](const DesignationType&) { return false; },
          [](const ObjectWriteType&) { return false; },
          [](const SampledHistoryType&) { return false; },
          [](const EvaluationAttemptsType&) { return false; },
          [](const EventType&) { return false; },
          [](const RuntimeEffectsType&) { return false; },
          [](const FilesType&) { return false; },
          [](const DiagnosticType&) { return false; },
          [](const RuntimeLibraryType&) { return false; },

          // A wildcard index (LRM 7.8.1) is a rule about which indices an array
          // admits rather than a type any value has, and `void` and a tagged
          // union's empty payload (LRM 7.3.2) are values no declaration holds
          // on its own.
          [](const WildcardIndexType&) { return false; },
          [](const EmptyType&) { return false; },
          [](const VoidType&) { return false; }});
}

auto Type::PartsAreStorage() const -> bool {
  return Visit(
      Overloaded{
          // Each element of an unpacked array, and each member of an unpacked
          // structure, is a place of its own that a reference can bind (LRM
          // 7.4, 7.8, 7.10, 13.5.2); a product the lowering composes is laid
          // out the same way.
          [](const UnpackedArrayType&) { return true; },
          [](const DynamicArrayType&) { return true; },
          [](const QueueType&) { return true; },
          [](const AssociativeArrayType&) { return true; },
          [](const TupleType&) { return true; },
          [](const StructType&) { return true; },

          // A packed value is one vector however its bits are named (LRM
          // 7.4.1), a string one sequence of characters (LRM 6.16), and a union
          // holds one member at a time over storage its members share (LRM
          // 7.3), so a part of any of them is a view of the whole.
          [](const PackedArrayType&) { return false; },
          [](const EnumType&) { return false; },
          [](const StringType&) { return false; },
          [](const UnionType&) { return false; },
          [](const TaggedUnionType&) { return false; },

          // Everything else has no parts a write reaches.
          [](const RealType&) { return false; },
          [](const ShortRealType&) { return false; },
          [](const ManagedRefType&) { return false; },
          [](const ChandleType&) { return false; },
          [](const MachineCStringType&) { return false; },
          [](const MachineBoolType&) { return false; },
          [](const MachineIntType&) { return false; },
          [](const MachineFloatType&) { return false; },
          [](const MachineArrayType&) { return false; },
          [](const MachineFunctionType&) { return false; },
          [](const RefType&) { return false; },
          [](const PointerType&) { return false; },
          [](const VectorType&) { return false; },
          [](const DriverType&) { return false; },
          [](const CoroutineType&) { return false; },
          [](const ObjectType&) { return false; },
          [](const RuntimeClassType&) { return false; },
          [](const ClosureType&) { return false; },
          [](const ObservableType&) { return false; },
          [](const ResolvedType&) { return false; },
          [](const OpenWriteType&) { return false; },
          [](const DesignationType&) { return false; },
          [](const ObjectWriteType&) { return false; },
          [](const SampledHistoryType&) { return false; },
          [](const EvaluationAttemptsType&) { return false; },
          [](const EventType&) { return false; },
          [](const RuntimeEffectsType&) { return false; },
          [](const FilesType&) { return false; },
          [](const DiagnosticType&) { return false; },
          [](const RuntimeLibraryType&) { return false; },
          [](const WildcardIndexType&) { return false; },
          [](const EmptyType&) { return false; },
          [](const VoidType&) { return false; }});
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

          // Held all at once, or one at a time, but never as a run of one
          // type.
          [](const TupleType&) -> Element { return std::nullopt; },
          [](const StructType&) -> Element { return std::nullopt; },
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
          [](const ChandleType&) -> Element { return std::nullopt; },
          [](const EventType&) -> Element { return std::nullopt; },
          [](const EmptyType&) -> Element { return std::nullopt; },
          [](const VoidType&) -> Element { return std::nullopt; },

          // A cell keeps one value, whatever it publishes about changes to it;
          // the run, where there is one, belongs to the value it keeps.
          [](const ObservableType&) -> Element { return std::nullopt; },
          [](const ResolvedType&) -> Element { return std::nullopt; },
          [](const DriverType&) -> Element { return std::nullopt; },
          [](const OpenWriteType&) -> Element { return std::nullopt; },
          [](const DesignationType&) -> Element { return std::nullopt; },
          [](const ObjectWriteType&) -> Element { return std::nullopt; },
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

          // An object and a closure are declarations, and a declaration is not
          // a run of anything.
          [](const ObjectType&) -> Element { return std::nullopt; },
          [](const RuntimeClassType&) -> Element { return std::nullopt; },
          [](const ClosureType&) -> Element { return std::nullopt; },

          // A handle to a runtime facility, and an inert payload the library
          // owns the shape of.
          [](const RuntimeEffectsType&) -> Element { return std::nullopt; },
          [](const FilesType&) -> Element { return std::nullopt; },
          [](const DiagnosticType&) -> Element { return std::nullopt; },
          [](const RuntimeLibraryType&) -> Element { return std::nullopt; }});
}

}  // namespace lyra::mir
