#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/structural_scope_id.hpp"

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
          [](const OwningPtrType&) { return TypeKind::kOwningPtr; },
          [](const VectorType&) { return TypeKind::kVector; },
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

namespace {

auto AsObjectClass(const CompilationUnit& unit, TypeId type)
    -> std::optional<StructuralScopeId> {
  const auto* obj = std::get_if<ObjectType>(&unit.GetType(type).data);
  if (obj == nullptr) {
    return std::nullopt;
  }
  return obj->target;
}

}  // namespace

auto IsOwningObjectType(const CompilationUnit& unit, TypeId type) -> bool {
  const auto* owning = std::get_if<OwningPtrType>(&unit.GetType(type).data);
  if (owning == nullptr) {
    return false;
  }
  return AsObjectClass(unit, owning->pointee).has_value();
}

auto IsVectorOfOwningObjectType(const CompilationUnit& unit, TypeId type)
    -> bool {
  const auto* vec = std::get_if<VectorType>(&unit.GetType(type).data);
  if (vec == nullptr) {
    return false;
  }
  return IsOwningObjectType(unit, vec->element);
}

auto GetOwnedObjectTarget(const CompilationUnit& unit, TypeId type)
    -> std::optional<StructuralScopeId> {
  const auto& data = unit.GetType(type).data;
  if (const auto* owning = std::get_if<OwningPtrType>(&data)) {
    return AsObjectClass(unit, owning->pointee);
  }
  if (const auto* vec = std::get_if<VectorType>(&data)) {
    if (const auto* owning =
            std::get_if<OwningPtrType>(&unit.GetType(vec->element).data)) {
      return AsObjectClass(unit, owning->pointee);
    }
  }
  return std::nullopt;
}

}  // namespace lyra::mir
