#include "lyra/value/fixed_array_type.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

auto LayoutOf(const ValueType& element, std::size_t count) -> SequenceLayout {
  return SequenceLayoutOf(element.Layout(), count);
}

}  // namespace

FixedArrayType::FixedArrayType(
    const ValueWitnessTable& witnesses, const value::ElementType& element,
    std::uint32_t count)
    : FixedArrayType(
          witnesses, element, count, LayoutOf(element.Type(), count)) {
}

FixedArrayType::FixedArrayType(
    const ValueWitnessTable& witnesses, const value::ElementType& element,
    std::uint32_t count, SequenceLayout layout)
    : type_(
          witnesses, static_cast<std::uint32_t>(layout.whole.size),
          static_cast<std::uint32_t>(layout.whole.align),
          ValueTypeKind::kFixedArray),
      count_(count),
      element_(&element) {
}

auto FixedArrayType::Stride() const -> std::size_t {
  return LayoutOf(element_->Type(), count_).stride;
}

auto FixedArrayType::GetUnchecked(const void* array, std::size_t index) const
    -> const void* {
  if (index >= count_) {
    throw InternalError(
        "the element of a fixed-size array is asked at an index past its "
        "last -- please report this as a bug");
  }
  return Offset(array, index * Stride());
}

auto FixedArrayType::GetUnchecked(void* array, std::size_t index) const
    -> void* {
  if (index >= count_) {
    throw InternalError(
        "the element of a fixed-size array is asked at an index past its "
        "last -- please report this as a bug");
  }
  return Offset(array, index * Stride());
}

auto FixedArrayType::ElementMut(
    void* array, std::optional<std::int64_t> position, Formation& formed) const
    -> void* {
  const std::optional<std::size_t> ordinal = ElementOrdinal(position, count_);
  formed = ordinal ? Formation::kExisting : Formation::kNowhere;
  return ordinal ? GetUnchecked(array, *ordinal) : element_->DiscardedWrite();
}

FixedArrayType::Building::Building(const FixedArrayType& type, void* out)
    : type_(&type), out_(out) {
}

FixedArrayType::Building::~Building() {
  if (finished_) {
    return;
  }
  for (std::size_t i = 0; i < built_; ++i) {
    type_->ElementType().Type().Destroy(Offset(out_, i * type_->Stride()));
  }
}

auto FixedArrayType::Building::Next() -> void* {
  return Offset(out_, built_ * type_->Stride());
}

void FixedArrayType::Building::Advance() {
  ++built_;
}

void FixedArrayType::Building::Finish() {
  finished_ = true;
}

auto ByteStringOf(const FixedArrayView& array) -> String {
  const IntegralValueType* byte_type = array.ElementType().Type().AsIntegral();
  if (byte_type == nullptr) {
    throw InternalError(
        "ByteStringOf: a byte array holds integral elements -- please report "
        "this as a bug");
  }
  const std::size_t count = array.Count();
  std::string text;
  text.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    const LoadedWords element = byte_type->Load(array.GetUnchecked(i));
    text.push_back(static_cast<char>(ToInt64(element.View()) & 0xFF));
  }
  return String{std::move(text)};
}

}  // namespace lyra::value
