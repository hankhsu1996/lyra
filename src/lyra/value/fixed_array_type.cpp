#include "lyra/value/fixed_array_type.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

namespace {

auto LayoutOf(const ValueType& element, std::size_t count) -> SequenceLayout {
  return SequenceLayoutOf(element.Layout(), count);
}

// The elements of one array as the sequence algorithms read them, and write
// them where the array may be written.
template <typename Bytes>
class Elements {
 public:
  Elements(const FixedArrayType& type, Bytes* array)
      : type_(&type), array_(array), element_(type.ElementType()) {
  }

  [[nodiscard]] auto Element() const -> const WitnessedElem& {
    return element_;
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return type_->Count();
  }
  [[nodiscard]] auto At(std::size_t i) const -> Bytes* {
    return type_->ElementAt(array_, i);
  }

 private:
  const FixedArrayType* type_;
  Bytes* array_;
  WitnessedElem element_;
};

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

auto FixedArrayType::ElementAt(const void* array, std::size_t position) const
    -> const void* {
  if (position >= count_) {
    throw InternalError(
        "FixedArrayType::ElementAt: the position is past the last");
  }
  return Offset(array, position * Stride());
}

auto FixedArrayType::ElementAt(void* array, std::size_t position) const
    -> void* {
  if (position >= count_) {
    throw InternalError(
        "FixedArrayType::ElementAt: the position is past the last");
  }
  return Offset(array, position * Stride());
}

auto FixedArrayType::ElementMut(
    void* array, std::optional<std::int64_t> position, Formation& formed) const
    -> void* {
  const std::optional<std::size_t> ordinal = ElementOrdinal(position, count_);
  formed = ordinal ? Formation::kExisting : Formation::kNowhere;
  return ordinal ? ElementAt(array, *ordinal) : element_->DiscardedWrite();
}

auto FixedArrayType::SliceElements(
    const void* array, std::optional<std::int64_t> start,
    std::size_t count) const -> std::vector<const void*> {
  return detail::SequenceSliceElements(Elements(*this, array), start, count);
}

auto FixedArrayType::AssignSlice(
    void* array, std::optional<std::int64_t> start, std::size_t count,
    std::span<const void* const> replacement) const -> bool {
  Elements elements(*this, array);
  return detail::SequenceAssignSlice(elements, start, count, replacement);
}

void FixedArrayType::Permute(
    void* array, std::span<const std::size_t> order) const {
  const AnyValue held = AnyValue::CopyOf(type_, array);
  Elements elements(*this, array);
  detail::SequencePermute(elements, Elements(*this, held.Bytes()), order);
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

namespace {

auto ArrayOf(const ValueType& type) -> const FixedArrayType& {
  return *type.AsFixedArray();
}

}  // namespace

extern "C" {
constinit const PartsWitnessTable lyra_rt_fixed_array_parts_witness_table{
    .count = [](const ValueType& type, const void*) -> std::size_t {
      return ArrayOf(type).Count();
    },
    .type = [](const ValueType& type, const void*) -> const ValueType& {
      return ArrayOf(type).ElementType().Type();
    },
    .at = [](const ValueType& type, const void* value, std::size_t position)
        -> const void* { return ArrayOf(type).ElementAt(value, position); },
    .ref_at = [](const ValueType& type, void* value, std::size_t position)
        -> void* { return ArrayOf(type).ElementAt(value, position); }};
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
    const LoadedWords element = byte_type->Load(array.ElementAt(i));
    text.push_back(static_cast<char>(ToInt64(element.View()) & 0xFF));
  }
  return String{std::move(text)};
}

}  // namespace lyra::value
