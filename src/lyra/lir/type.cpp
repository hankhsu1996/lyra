#include "lyra/lir/type.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

namespace {

void MixBits(std::size_t& seed, std::size_t value) {
  seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

void Combine(std::size_t& seed, std::uint64_t value) {
  MixBits(seed, std::hash<std::uint64_t>{}(value));
}

void Combine(std::size_t& seed, const std::string& value) {
  MixBits(seed, std::hash<std::string>{}(value));
}

void Combine(std::size_t& seed, TypeId id) {
  Combine(seed, id.value);
}

void Combine(std::size_t& seed, ClassId id) {
  Combine(seed, id.value);
}

template <typename E>
  requires std::is_enum_v<E>
void Combine(std::size_t& seed, E value) {
  Combine(seed, static_cast<std::uint64_t>(value));
}

void Combine(std::size_t& seed, const std::vector<TypeId>& ids) {
  for (const TypeId id : ids) {
    Combine(seed, id);
  }
}

void Combine(std::size_t& seed, const PackedArrayType& packed) {
  Combine(seed, packed.state_kind);
  Combine(seed, packed.signedness);
  for (const PackedRange& dim : packed.dims) {
    Combine(seed, static_cast<std::uint64_t>(dim.left));
    Combine(seed, static_cast<std::uint64_t>(dim.right));
  }
}

}  // namespace

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit(
      Overloaded{
          [&](const PackedArrayType& t) { Combine(seed, t); },
          [&](const EnumType& t) {
            Combine(seed, t.base);
            for (const EnumMember& member : t.members) {
              Combine(seed, member.name);
              Combine(seed, static_cast<std::uint64_t>(member.value));
            }
          },
          [&](const UnpackedArrayType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.size);
          },
          [&](const DynamicArrayType& t) { Combine(seed, t.element_type); },
          [&](const QueueType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.max_bound.value_or(0));
            Combine(seed, static_cast<std::uint64_t>(t.max_bound.has_value()));
          },
          [&](const AssociativeArrayType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.key_type);
          },
          [](const WildcardIndexType&) {},
          [](const StringType&) {},
          [](const MachineCStringType&) {},
          [](const MachineBoolType&) {},
          [&](const MachineIntType& t) {
            Combine(seed, t.bit_width);
            Combine(seed, t.signedness);
          },
          [&](const MachineFloatType& t) { Combine(seed, t.bit_width); },
          [&](const MachineArrayType& t) {
            Combine(seed, t.element);
            Combine(seed, t.size);
          },
          [](const EventType&) {},
          [](const RealType&) {},
          [](const ShortRealType&) {},
          [](const RealTimeType&) {},
          [](const ChandleType&) {},
          [](const VoidType&) {},
          [](const EmptyType&) {},
          [&](const ObjectType& t) { Combine(seed, t.class_id); },
          [&](const ExternalUnitObjectType& t) {
            Combine(seed, t.object.value);
          },
          [&](const CrossUnitClassType& t) {
            Combine(seed, t.unit_name);
            Combine(seed, t.class_name);
          },
          [&](const RuntimeClassType& t) { Combine(seed, t.symbol); },
          [&](const ClosureType& t) { Combine(seed, t.closure_id.value); },
          [](const RuntimeEffectsType&) {},
          [](const FilesType&) {},
          [](const DiagnosticType&) {},
          [&](const RuntimeLibraryType& t) { Combine(seed, t.kind); },
          [&](const CoroutineType& t) { Combine(seed, t.payload); },
          [&](const RefType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.mutability);
          },
          [&](const PointerType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.ownership);
            Combine(seed, t.mutability);
          },
          [&](const ManagedRefType& t) { Combine(seed, t.pointee); },
          [&](const VectorType& t) { Combine(seed, t.element); },
          [&](const TupleType& t) { Combine(seed, t.elements); },
          [&](const UnionType& t) { Combine(seed, t.elements); },
          [&](const TaggedUnionType& t) { Combine(seed, t.elements); },
          [&](const ResolvedType& t) {
            Combine(seed, t.value);
            Combine(seed, t.resolution);
          },
          [&](const DriverType& t) {
            Combine(seed, t.value);
            Combine(seed, t.resolution);
          },
          [&](const ObservableType& t) { Combine(seed, t.value); }});
  return seed;
}

auto Type::KindName() const -> std::string_view {
  return Visit(
      Overloaded{
          [](const PackedArrayType&) { return "packed array"; },
          [](const EnumType&) { return "enumeration"; },
          [](const UnpackedArrayType&) { return "unpacked array"; },
          [](const DynamicArrayType&) { return "dynamic array"; },
          [](const QueueType&) { return "queue"; },
          [](const AssociativeArrayType&) { return "associative array"; },
          [](const WildcardIndexType&) { return "wildcard index"; },
          [](const StringType&) { return "string"; },
          [](const MachineCStringType&) { return "machine C string"; },
          [](const MachineBoolType&) { return "machine boolean"; },
          [](const MachineIntType&) { return "machine integer"; },
          [](const MachineFloatType&) { return "machine float"; },
          [](const MachineArrayType&) { return "machine array"; },
          [](const EventType&) { return "named event"; },
          [](const RealType&) { return "real"; },
          [](const ShortRealType&) { return "shortreal"; },
          [](const RealTimeType&) { return "realtime"; },
          [](const ChandleType&) { return "chandle"; },
          [](const VoidType&) { return "void"; },
          [](const EmptyType&) { return "empty"; },
          [](const ObjectType&) { return "class object"; },
          [](const ExternalUnitObjectType&) { return "external unit object"; },
          [](const CrossUnitClassType&) { return "cross-unit class"; },
          [](const RuntimeClassType&) { return "runtime class"; },
          [](const ClosureType&) { return "closure"; },
          [](const RuntimeEffectsType&) { return "runtime services"; },
          [](const FilesType&) { return "file table"; },
          [](const DiagnosticType&) { return "diagnostic dispatcher"; },
          [](const RuntimeLibraryType&) { return "runtime library value"; },
          [](const CoroutineType&) { return "coroutine"; },
          [](const RefType&) { return "reference"; },
          [](const PointerType&) { return "pointer"; },
          [](const ManagedRefType&) { return "managed reference"; },
          [](const VectorType&) { return "vector"; },
          [](const TupleType&) { return "product"; },
          [](const UnionType&) { return "union"; },
          [](const TaggedUnionType&) { return "tagged union"; },
          [](const ResolvedType&) { return "net resolution node"; },
          [](const DriverType&) { return "net driver"; },
          [](const ObservableType&) { return "observable cell"; }});
}

auto Type::Pointee() const -> std::optional<TypeId> {
  if (const auto* pointer = As<PointerType>()) {
    return pointer->pointee;
  }
  if (const auto* reference = As<RefType>()) {
    return reference->pointee;
  }
  if (const auto* managed = As<ManagedRefType>()) {
    return managed->pointee;
  }
  return std::nullopt;
}

auto Type::DerefTarget() const -> std::optional<TypeId> {
  if (const auto* observable = As<ObservableType>()) {
    return observable->value;
  }
  if (const auto* resolved = As<ResolvedType>()) {
    return resolved->value;
  }
  if (const auto* driver = As<DriverType>()) {
    return driver->value;
  }
  return Pointee();
}

auto Type::IsAddressOnly() const -> bool {
  return Is<ObservableType>() || Is<ResolvedType>() || Is<ObjectType>() ||
         Is<ExternalUnitObjectType>() || Is<CrossUnitClassType>() ||
         Is<RuntimeClassType>();
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
  throw InternalError("lir: type has no packed shape; it is not integral");
}

}  // namespace lyra::lir
