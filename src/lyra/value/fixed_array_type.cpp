#include "lyra/value/fixed_array_type.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <optional>
#include <span>
#include <string>
#include <typeinfo>
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
#include "lyra/value/position.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

namespace {

auto LayoutOf(const ValueType& element, std::size_t count) -> FixedArrayLayout {
  return FixedArrayLayoutOf(element.Size(), element.Align(), count);
}

auto Offset(const void* from, std::size_t bytes) -> const void* {
  return std::bit_cast<const void*>(
      std::bit_cast<std::uintptr_t>(from) + bytes);
}
auto Offset(void* from, std::size_t bytes) -> void* {
  return std::bit_cast<void*>(std::bit_cast<std::uintptr_t>(from) + bytes);
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

[[noreturn]] void NeverAskedOfAnArray() {
  throw InternalError(
      "FixedArrayType: an operation is asked of an array that the language "
      "defines for no array -- please report this as a bug");
}

}  // namespace

FixedArrayType::FixedArrayType(
    const value::ElementType& element, std::uint32_t count)
    : ValueType(
          static_cast<std::uint32_t>(LayoutOf(element.Type(), count).size),
          static_cast<std::uint32_t>(LayoutOf(element.Type(), count).align)),
      count_(count),
      element_(&element),
      parts_(*this) {
}

auto FixedArrayType::Layout() const -> FixedArrayLayout {
  return LayoutOf(element_->Type(), count_);
}

auto FixedArrayType::ElementAt(const void* array, std::size_t position) const
    -> const void* {
  if (position >= count_) {
    throw InternalError(
        "FixedArrayType::ElementAt: the position is past the last");
  }
  return Offset(array, position * Layout().stride);
}

auto FixedArrayType::ElementAt(void* array, std::size_t position) const
    -> void* {
  if (position >= count_) {
    throw InternalError(
        "FixedArrayType::ElementAt: the position is past the last");
  }
  return Offset(array, position * Layout().stride);
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
  const AnyValue held = AnyValue::CopyOf(*this, array);
  Elements elements(*this, array);
  detail::SequencePermute(elements, Elements(*this, held.Bytes()), order);
}

auto FixedArrayType::CountStatedAt() const -> StatedAt {
  return StatedAtIn(this, count_);
}

auto FixedArrayType::ElementStatedAt() const -> StatedAt {
  return StatedAtIn(this, element_);
}

// What the parts object states first is its table, the one address an object
// of a class with virtual members opens with.
auto FixedArrayType::PartsStatedAt() const -> StatedAt {
  return StatedAt{
      .offset = StatedAtIn(this, parts_).offset, .bytes = sizeof(void*)};
}

auto FixedArrayType::PartsOwnerStatedAt() const -> StatedAt {
  return StatedAtIn(this, parts_.array_);
}

auto FixedArrayType::PartsClassName() -> const char* {
  return typeid(ElementsByPosition).name();
}

FixedArrayType::Building::Building(const FixedArrayType& type, void* out)
    : type_(&type), out_(out) {
}

FixedArrayType::Building::~Building() {
  if (finished_) {
    return;
  }
  for (std::size_t i = 0; i < built_; ++i) {
    type_->ElementType().Type().Destroy(
        Offset(out_, i * type_->Layout().stride));
  }
}

auto FixedArrayType::Building::Next() -> void* {
  return Offset(out_, built_ * type_->Layout().stride);
}

void FixedArrayType::Building::Advance() {
  ++built_;
}

void FixedArrayType::Building::Finish() {
  finished_ = true;
}

void FixedArrayType::Copy(const void* value, void* out) const {
  BuildEach(out, [&](std::size_t i, void* element) {
    element_->Type().Copy(ElementAt(value, i), element);
  });
}

void FixedArrayType::Move(void* value, void* out) const noexcept {
  for (std::size_t i = 0; i < count_; ++i) {
    element_->Type().Move(
        Offset(value, i * Layout().stride), Offset(out, i * Layout().stride));
  }
}

void FixedArrayType::Destroy(void* value) const noexcept {
  for (std::size_t i = 0; i < count_; ++i) {
    element_->Type().Destroy(Offset(value, i * Layout().stride));
  }
}

void FixedArrayType::Assign(void* storage, const void* value) const {
  for (std::size_t i = 0; i < count_; ++i) {
    element_->Type().Assign(ElementAt(storage, i), ElementAt(value, i));
  }
}

auto FixedArrayType::Equal(const void* lhs, const void* rhs) const
    -> FourStateBit {
  return detail::SequenceEqual(Elements(*this, lhs), Elements(*this, rhs));
}

auto FixedArrayType::CaseEqual(const void* lhs, const void* rhs) const -> bool {
  return detail::SequenceCaseEqual(Elements(*this, lhs), Elements(*this, rhs))
      .IsTruthy();
}

auto FixedArrayType::BitIdentical(const void* lhs, const void* rhs) const
    -> bool {
  return detail::SequenceBitIdentical(
      Elements(*this, lhs), Elements(*this, rhs));
}

auto FixedArrayType::HasUnknown(const void* value) const -> bool {
  return detail::SequenceHasUnknown(Elements(*this, value));
}

void FixedArrayType::BuildDefault(void* out) const {
  BuildEach(out, [&](std::size_t, void* element) {
    element_->Type().BuildDefault(element);
  });
}

void FixedArrayType::BitstreamWidth(const void* value, void* out) const {
  std::construct_at(
      static_cast<Int*>(out),
      Int::FromInt(detail::SequenceBitstreamWidth(Elements(*this, value))));
}

void FixedArrayType::CountBits(
    const void* value, const ConstIntegralView& control_bits, void* out) const {
  std::construct_at(
      static_cast<Int*>(out),
      Int::FromInt(
          detail::SequenceCountBits(Elements(*this, value), control_bits)));
}

// LRM 6.24.3: the elements' own streams laid end to end, the one at position 0
// most significant.
auto FixedArrayType::WriteToStream(
    const void* value, const IntegralView& stream, std::uint64_t filled) const
    -> std::uint64_t {
  for (std::size_t i = 0; i < count_; ++i) {
    filled =
        element_->Type().WriteToStream(ElementAt(value, i), stream, filled);
  }
  return filled;
}

auto FixedArrayType::ReadFromStream(
    const ConstIntegralView& stream, std::uint64_t taken, void* out) const
    -> std::uint64_t {
  BuildEach(out, [&](std::size_t, void* element) {
    taken = element_->Type().ReadFromStream(stream, taken, element);
  });
  return taken;
}

void FixedArrayType::FoldedBy(
    Fold fold, const void* lhs, const void* rhs, void* out) const {
  BuildEach(out, [&](std::size_t i, void* element) {
    (element_->Type().*fold)(ElementAt(lhs, i), ElementAt(rhs, i), element);
  });
}

void FixedArrayType::ResolveTriState(
    const void* lhs, const void* rhs, void* out) const {
  FoldedBy(&ValueType::ResolveTriState, lhs, rhs, out);
}

void FixedArrayType::ResolveWiredAnd(
    const void* lhs, const void* rhs, void* out) const {
  FoldedBy(&ValueType::ResolveWiredAnd, lhs, rhs, out);
}

void FixedArrayType::ResolveWiredOr(
    const void* lhs, const void* rhs, void* out) const {
  FoldedBy(&ValueType::ResolveWiredOr, lhs, rhs, out);
}

void FixedArrayType::Dominating(
    const void* stronger, const void* weaker, void* out) const {
  FoldedBy(&ValueType::Dominating, stronger, weaker, out);
}

void FixedArrayType::FilledLike(
    const void* prototype, const void* fill, void* out) const {
  BuildEach(out, [&](std::size_t i, void* element) {
    element_->Type().FilledLike(ElementAt(prototype, i), fill, element);
  });
}

auto FixedArrayType::OrderBefore(const void*, const void*) const -> bool {
  NeverAskedOfAnArray();
}

auto FixedArrayType::IsTrue(const void*) const -> bool {
  NeverAskedOfAnArray();
}

void FixedArrayType::Reduce(Reduction, const void*, const void*, void*) const {
  NeverAskedOfAnArray();
}

auto FixedArrayType::Parts() const -> const PartsByPosition* {
  return &parts_;
}

auto FixedArrayType::AsIntegral() const -> const IntegralValueType* {
  return nullptr;
}

auto FixedArrayType::AsFixedArray() const -> const FixedArrayType* {
  return this;
}

auto FixedArrayType::ElementsByPosition::Count(const void*) const
    -> std::size_t {
  return array_->Count();
}

auto FixedArrayType::ElementsByPosition::Type(const void*) const
    -> const ValueType& {
  return array_->ElementType().Type();
}

auto FixedArrayType::ElementsByPosition::At(
    const void* value, std::size_t position) const -> const void* {
  return array_->ElementAt(value, position);
}

auto FixedArrayType::ElementsByPosition::RefAt(
    void* value, std::size_t position) const -> void* {
  return array_->ElementAt(value, position);
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
