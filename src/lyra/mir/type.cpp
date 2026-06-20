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
          [](const StringType&) { return TypeKind::kString; },
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
          [](const SelfType&) { return TypeKind::kSelf; },
          [](const ServicesType&) { return TypeKind::kServices; },
          [](const RuntimeLibraryType&) { return TypeKind::kRuntimeLibrary; },
          [](const CoroutineType&) { return TypeKind::kCoroutine; },
          [](const RefType&) { return TypeKind::kReference; },
          [](const PointerType&) { return TypeKind::kPointer; },
          [](const VectorType&) { return TypeKind::kVector; },
          [](const ExternalRefType&) { return TypeKind::kExternalRef; },
          [](const ObservableType&) { return TypeKind::kObservable; },
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
  const auto* ptr = std::get_if<PointerType>(&unit.GetType(type).data);
  if (ptr == nullptr || ptr->ownership != PointerOwnership::kUnique) {
    return std::nullopt;
  }
  return ptr->pointee;
}

}  // namespace

auto IsObservableCellType(const Type& ty) -> bool {
  return std::holds_alternative<ObservableType>(ty.data) ||
         std::holds_alternative<ExternalRefType>(ty.data);
}

auto ObservableInnerValueType(const Type& ty) -> TypeId {
  if (const auto* ob = std::get_if<ObservableType>(&ty.data)) {
    return ob->value;
  }
  if (const auto* er = std::get_if<ExternalRefType>(&ty.data)) {
    return er->element;
  }
  throw InternalError(
      "ObservableInnerValueType: type is not an observable cell wrapper");
}

auto GetChildScope(const CompilationUnit& unit, TypeId type)
    -> std::optional<ChildScope> {
  TypeId leaf = type;
  while (const auto* vec = std::get_if<VectorType>(&unit.GetType(leaf).data)) {
    leaf = vec->element;
  }
  const auto pointee = AsUniquePointee(unit, leaf);
  if (!pointee.has_value()) {
    return std::nullopt;
  }
  const auto& data = unit.GetType(*pointee).data;
  if (const auto* obj = std::get_if<ObjectType>(&data)) {
    return ChildScope{GenerateScopeChild{.target = obj->target}};
  }
  if (std::holds_alternative<ExternalUnitObjectType>(data)) {
    return ChildScope{ModuleInstanceChild{}};
  }
  return std::nullopt;
}

}  // namespace lyra::mir
