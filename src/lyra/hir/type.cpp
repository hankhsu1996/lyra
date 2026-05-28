#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstddef>
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

auto PackedArrayType::DefaultInitialValue() const -> IntegralConstant {
  const auto width = static_cast<std::uint32_t>(BitWidth());
  const std::size_t words = (static_cast<std::size_t>(width) + 63U) / 64U;
  if (!IsFourState()) {
    return IntegralConstant{
        .value_words = std::vector<std::uint64_t>(words, 0U),
        .state_words = {},
        .width = width,
        .signedness = signedness,
        .state_kind = IntegralStateKind::kTwoState,
    };
  }
  std::vector<std::uint64_t> v(words, ~std::uint64_t{0});
  std::vector<std::uint64_t> s(words, ~std::uint64_t{0});
  const std::uint32_t tail = width % 64U;
  if (tail != 0U && !v.empty()) {
    const std::uint64_t mask = (std::uint64_t{1} << tail) - 1U;
    v.back() &= mask;
    s.back() &= mask;
  }
  return IntegralConstant{
      .value_words = std::move(v),
      .state_words = std::move(s),
      .width = width,
      .signedness = signedness,
      .state_kind = IntegralStateKind::kFourState,
  };
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

}  // namespace lyra::hir
