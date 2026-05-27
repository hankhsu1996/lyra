#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/method.hpp"

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

auto Type::GetMethods() const -> std::span<const Method> {
  return std::visit(
      Overloaded{
          [](const EnumType& e) -> std::span<const Method> {
            return e.methods;
          },
          [](const auto&) -> std::span<const Method> { return {}; },
      },
      data);
}

auto Type::GetMethod(MethodId id) const -> const Method& {
  const auto methods = GetMethods();
  if (id.value >= methods.size()) {
    throw InternalError("Type::GetMethod: MethodId out of range");
  }
  return methods[id.value];
}

auto Type::LookupMethod(std::string_view name) const
    -> std::optional<MethodId> {
  const auto methods = GetMethods();
  for (std::size_t i = 0; i < methods.size(); ++i) {
    if (methods[i].name == name) {
      return MethodId{static_cast<std::uint32_t>(i)};
    }
  }
  return std::nullopt;
}

}  // namespace lyra::hir
