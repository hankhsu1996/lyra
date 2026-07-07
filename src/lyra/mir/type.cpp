#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"

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

auto PackedRange::LinearOffset(std::int64_t index) const -> std::uint64_t {
  if (!Contains(index)) {
    throw InternalError("PackedRange::LinearOffset: index out of range");
  }
  if (left >= right) {
    return static_cast<std::uint64_t>(left - index);
  }
  return static_cast<std::uint64_t>(index - left);
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

auto PackedArrayType::IsFourState() const -> bool {
  return atom == BitAtom::kLogic || atom == BitAtom::kReg;
}

auto Type::Kind() const -> TypeKind {
  return std::visit(
      Overloaded{
          [](const PackedArrayType&) { return TypeKind::kPackedArray; },
          [](const EnumType&) { return TypeKind::kEnum; },
          [](const UnpackedArrayType&) { return TypeKind::kUnpackedArray; },
          [](const DynamicArrayType&) { return TypeKind::kDynamicArray; },
          [](const QueueType&) { return TypeKind::kQueue; },
          [](const AssociativeArrayType&) {
            return TypeKind::kAssociativeArray;
          },
          [](const WildcardIndexType&) { return TypeKind::kWildcardIndex; },
          [](const StringType&) { return TypeKind::kString; },
          [](const StringViewType&) { return TypeKind::kStringView; },
          [](const MachineIntType&) { return TypeKind::kMachineInt; },
          [](const EventType&) { return TypeKind::kEvent; },
          [](const RealType&) { return TypeKind::kReal; },
          [](const ShortRealType&) { return TypeKind::kShortReal; },
          [](const RealTimeType&) { return TypeKind::kRealTime; },
          [](const ChandleType&) { return TypeKind::kChandle; },
          [](const VoidType&) { return TypeKind::kVoid; },
          [](const ObjectType&) { return TypeKind::kObject; },
          [](const ExternalUnitObjectType&) {
            return TypeKind::kExternalUnitObject;
          },
          [](const ScopeType&) { return TypeKind::kScope; },
          [](const InstanceType&) { return TypeKind::kInstance; },
          [](const GenScopeType&) { return TypeKind::kGenScope; },
          [](const ProceduralStorageScopeType&) {
            return TypeKind::kProceduralStorageScope;
          },
          [](const ServicesType&) { return TypeKind::kServices; },
          [](const FilesType&) { return TypeKind::kFiles; },
          [](const DiagnosticType&) { return TypeKind::kDiagnostic; },
          [](const RuntimeLibraryType&) { return TypeKind::kRuntimeLibrary; },
          [](const CoroutineType&) { return TypeKind::kCoroutine; },
          [](const RefType&) { return TypeKind::kReference; },
          [](const PointerType&) { return TypeKind::kPointer; },
          [](const ManagedRefType&) { return TypeKind::kManagedRef; },
          [](const VectorType&) { return TypeKind::kVector; },
          [](const TupleType&) { return TypeKind::kTuple; },
          [](const UnionType&) { return TypeKind::kUnion; },
          [](const ObservableType&) { return TypeKind::kObservable; },
          [](const ResolvedType&) { return TypeKind::kResolved; },
          [](const DriverType&) { return TypeKind::kDriver; },
          [](const StructType&) { return TypeKind::kStruct; },
          [](const ClosureType&) { return TypeKind::kClosure; },
      },
      data);
}

auto Type::IsPackedArray() const -> bool {
  return std::holds_alternative<PackedArrayType>(data);
}

auto Type::AsPackedArray() const -> const PackedArrayType& {
  if (const auto* p = std::get_if<PackedArrayType>(&data)) {
    return *p;
  }
  throw InternalError("Type::AsPackedArray called on non-packed-array type");
}

auto Type::IsEnum() const -> bool {
  return std::holds_alternative<EnumType>(data);
}

auto Type::AsEnum() const -> const EnumType& {
  if (const auto* e = std::get_if<EnumType>(&data)) {
    return *e;
  }
  throw InternalError("Type::AsEnum called on non-enum type");
}

auto Type::IsIntegralPacked() const -> bool {
  return std::holds_alternative<PackedArrayType>(data) ||
         std::holds_alternative<EnumType>(data);
}

auto Type::AsIntegralPacked() const -> const PackedArrayType& {
  if (const auto* p = std::get_if<PackedArrayType>(&data)) {
    return *p;
  }
  if (const auto* e = std::get_if<EnumType>(&data)) {
    return e->base;
  }
  throw InternalError(
      "Type::AsIntegralPacked called on non-integral type (expected "
      "PackedArrayType or EnumType)");
}

namespace {

auto AsUniquePointee(const CompilationUnit& unit, TypeId type)
    -> std::optional<TypeId> {
  const auto* ptr = std::get_if<PointerType>(&unit.types.Get(type).data);
  if (ptr == nullptr || ptr->ownership != PointerOwnership::kUnique) {
    return std::nullopt;
  }
  return ptr->pointee;
}

}  // namespace

auto IsObservableCellType(const Type& ty) -> bool {
  return std::holds_alternative<ObservableType>(ty.data) ||
         std::holds_alternative<RefType>(ty.data) ||
         std::holds_alternative<ResolvedType>(ty.data);
}

auto ObservableInnerValueType(const Type& ty) -> TypeId {
  if (const auto* ob = std::get_if<ObservableType>(&ty.data)) {
    return ob->value;
  }
  if (const auto* rf = std::get_if<RefType>(&ty.data)) {
    return rf->pointee;
  }
  if (const auto* rn = std::get_if<ResolvedType>(&ty.data)) {
    return rn->value;
  }
  throw InternalError(
      "ObservableInnerValueType: type is not an observable cell wrapper");
}

auto GetChildScope(const CompilationUnit& unit, TypeId type)
    -> std::optional<ChildScope> {
  TypeId leaf = type;
  while (const auto* vec =
             std::get_if<VectorType>(&unit.types.Get(leaf).data)) {
    leaf = vec->element;
  }
  const auto pointee = AsUniquePointee(unit, leaf);
  if (!pointee.has_value()) {
    return std::nullopt;
  }
  const auto& data = unit.types.Get(*pointee).data;
  if (const auto* obj = std::get_if<ObjectType>(&data)) {
    return ChildScope{
        GenerateScopeChild{.name = unit.GetClass(obj->class_id).name}};
  }
  if (std::holds_alternative<ExternalUnitObjectType>(data)) {
    return ChildScope{ModuleInstanceChild{}};
  }
  return std::nullopt;
}

}  // namespace lyra::mir
