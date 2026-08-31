#include "lyra/lir/type_query.hpp"

#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

namespace {

// Which types are integral, and what structures their values' bits, answered
// once: a packed array is its own shape, and an enumeration is represented by
// its base's. Absent for every other type, which is what makes it not
// integral.
auto FindPackedShape(const TypeArena& types, TypeId type)
    -> const PackedArrayType* {
  const TypeData& data = types.Get(type).data;
  if (const auto* packed = std::get_if<PackedArrayType>(&data)) {
    return packed;
  }
  if (const auto* enumeration = std::get_if<EnumType>(&data)) {
    return &enumeration->base;
  }
  return nullptr;
}

}  // namespace

auto TypeKindName(const Type& type) -> std::string_view {
  return std::visit(
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
          [](const ObservableType&) { return "observable cell"; }},
      type.data);
}

auto Pointee(const TypeArena& types, TypeId type) -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [](const PointerType& p) -> std::optional<TypeId> {
            return p.pointee;
          },
          [](const RefType& r) -> std::optional<TypeId> { return r.pointee; },
          [](const ManagedRefType& m) -> std::optional<TypeId> {
            return m.pointee;
          },
          [](const auto&) -> std::optional<TypeId> { return std::nullopt; }},
      types.Get(type).data);
}

auto DerefTarget(const TypeArena& types, TypeId type) -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [](const ObservableType& o) -> std::optional<TypeId> {
            return o.value;
          },
          [](const ResolvedType& r) -> std::optional<TypeId> {
            return r.value;
          },
          [](const DriverType& d) -> std::optional<TypeId> { return d.value; },
          [&](const auto&) -> std::optional<TypeId> {
            return Pointee(types, type);
          }},
      types.Get(type).data);
}

auto IsAddressOnly(const TypeArena& types, TypeId type) -> bool {
  return std::visit(
      Overloaded{
          [](const ObservableType&) { return true; },
          [](const ResolvedType&) { return true; },
          [](const ObjectType&) { return true; },
          [](const ExternalUnitObjectType&) { return true; },
          [](const CrossUnitClassType&) { return true; },
          [](const RuntimeClassType&) { return true; },
          [](const auto&) { return false; }},
      types.Get(type).data);
}

auto IsIntegral(const TypeArena& types, TypeId type) -> bool {
  return FindPackedShape(types, type) != nullptr;
}

auto PackedShape(const TypeArena& types, TypeId type)
    -> const PackedArrayType& {
  const PackedArrayType* shape = FindPackedShape(types, type);
  if (shape == nullptr) {
    throw InternalError("lir: type has no packed shape; it is not integral");
  }
  return *shape;
}

auto SameRepresentation(const PackedArrayType& a, const PackedArrayType& b)
    -> bool {
  return a.atom == b.atom && a.signedness == b.signedness && a.dims == b.dims;
}

auto IsCoroutine(const TypeArena& types, TypeId type) -> bool {
  return std::holds_alternative<CoroutineType>(types.Get(type).data);
}

}  // namespace lyra::lir
