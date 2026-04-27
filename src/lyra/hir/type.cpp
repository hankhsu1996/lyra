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

}  // namespace lyra::hir
