#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstdint>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

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

auto PackedRange::LinearOffset(std::int64_t index) const -> std::uint64_t {
  if (!Contains(index)) {
    throw InternalError("PackedRange::LinearOffset: index out of range");
  }
  if (left >= right) {
    return static_cast<std::uint64_t>(left - index);
  }
  return static_cast<std::uint64_t>(index - left);
}

auto Type::Kind() const -> TypeKind {
  return std::visit(
      Overloaded{
          [](const ScalarBitType&) { return TypeKind::kScalarBit; },
          [](const PackedArrayType&) { return TypeKind::kPackedArray; },
          [](const PackedStructType&) { return TypeKind::kPackedStruct; },
          [](const PackedUnionType&) { return TypeKind::kPackedUnion; },
          [](const EnumType&) { return TypeKind::kEnum; },
          [](const UnpackedStructType&) { return TypeKind::kUnpackedStruct; },
          [](const UnpackedUnionType&) { return TypeKind::kUnpackedUnion; },
          [](const UnpackedArrayType&) { return TypeKind::kUnpackedArray; },
          [](const DynamicArrayType&) { return TypeKind::kDynamicArray; },
          [](const QueueType&) { return TypeKind::kQueue; },
          [](const AssociativeArrayType&) {
            return TypeKind::kAssociativeArray;
          },
          [](const WildcardIndexType&) { return TypeKind::kWildcardIndex; },
          [](const StringType&) { return TypeKind::kString; },
          [](const EventType&) { return TypeKind::kEvent; },
          [](const RealType&) { return TypeKind::kReal; },
          [](const ShortRealType&) { return TypeKind::kShortReal; },
          [](const RealTimeType&) { return TypeKind::kRealTime; },
          [](const ChandleType&) { return TypeKind::kChandle; },
          [](const ClassHandleType&) { return TypeKind::kClassHandle; },
          [](const NullType&) { return TypeKind::kNull; },
          [](const VoidType&) { return TypeKind::kVoid; },
      },
      data);
}

auto Type::IsScalarBit() const -> bool {
  return std::holds_alternative<ScalarBitType>(data);
}

auto Type::AsScalarBit() const -> const ScalarBitType& {
  if (const auto* s = std::get_if<ScalarBitType>(&data)) {
    return *s;
  }
  throw InternalError("Type::AsScalarBit called on non-scalar-bit type");
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

auto Type::IsBitVector() const -> bool {
  return IsScalarBit() || IsPackedArray();
}

auto Type::IsPackedStruct() const -> bool {
  return std::holds_alternative<PackedStructType>(data);
}

auto Type::AsPackedStruct() const -> const PackedStructType& {
  if (const auto* s = std::get_if<PackedStructType>(&data)) {
    return *s;
  }
  throw InternalError("Type::AsPackedStruct called on non-packed-struct type");
}

auto Type::IsPackedUnion() const -> bool {
  return std::holds_alternative<PackedUnionType>(data);
}

auto Type::AsPackedUnion() const -> const PackedUnionType& {
  if (const auto* u = std::get_if<PackedUnionType>(&data)) {
    return *u;
  }
  throw InternalError("Type::AsPackedUnion called on non-packed-union type");
}

auto Type::IsValueChangeObservable() const -> bool {
  switch (Kind()) {
    case TypeKind::kScalarBit:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kPackedUnion:
    case TypeKind::kEnum:
    case TypeKind::kUnpackedStruct:
    case TypeKind::kUnpackedUnion:
    case TypeKind::kUnpackedArray:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
    case TypeKind::kString:
    case TypeKind::kReal:
    case TypeKind::kShortReal:
    case TypeKind::kRealTime:
      return true;
    case TypeKind::kWildcardIndex:
    case TypeKind::kEvent:
    case TypeKind::kChandle:
    case TypeKind::kClassHandle:
    case TypeKind::kNull:
    case TypeKind::kVoid:
      return false;
  }
  return false;
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

}  // namespace lyra::hir
