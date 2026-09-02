#include "lyra/value/runtime_unpacked_array.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

RuntimeUnpackedArray::RuntimeUnpackedArray()
    : element_default_(std::make_unique<RuntimeValue>()) {
}

RuntimeUnpackedArray::RuntimeUnpackedArray(
    RuntimeValue element_default, std::vector<RuntimeValue> unit,
    std::size_t count)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))) {
  data_.reserve(unit.size() * count);
  for (std::size_t i = 0; i < count; ++i) {
    data_.insert(data_.end(), unit.begin(), unit.end());
  }
}

RuntimeUnpackedArray::RuntimeUnpackedArray(const RuntimeUnpackedArray& other)
    : element_default_(std::make_unique<RuntimeValue>(*other.element_default_)),
      data_(other.data_) {
}

RuntimeUnpackedArray::RuntimeUnpackedArray(RuntimeUnpackedArray&&) noexcept =
    default;

auto RuntimeUnpackedArray::operator=(const RuntimeUnpackedArray& other)
    -> RuntimeUnpackedArray& {
  if (this != &other) {
    element_default_ = std::make_unique<RuntimeValue>(*other.element_default_);
    data_ = other.data_;
  }
  return *this;
}

auto RuntimeUnpackedArray::operator=(RuntimeUnpackedArray&&) noexcept
    -> RuntimeUnpackedArray& = default;

RuntimeUnpackedArray::~RuntimeUnpackedArray() = default;

auto RuntimeUnpackedArray::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(data_.size()));
}

auto RuntimeUnpackedArray::ElementDefault() const -> const RuntimeValue& {
  return *element_default_;
}

auto RuntimeUnpackedArray::Element(
    const PackedArray& sv_index, const PackedArray& left,
    const PackedArray& right) const -> const RuntimeValue& {
  const std::optional<std::size_t> ordinal =
      ResolveUnpackedOrdinal(sv_index, left, right, data_.size());
  if (!ordinal) {
    return *element_default_;
  }
  return data_[*ordinal];
}

auto RuntimeUnpackedArray::ElementAt(std::size_t position) const
    -> const RuntimeValue& {
  if (position >= data_.size()) {
    throw InternalError(
        "RuntimeUnpackedArray::ElementAt: the position is past the last");
  }
  return data_[position];
}

auto RuntimeUnpackedArray::WithElement(
    const PackedArray& sv_index, const PackedArray& left,
    const PackedArray& right, RuntimeValue value) const
    -> RuntimeUnpackedArray {
  RuntimeUnpackedArray result(*this);
  const std::optional<std::size_t> ordinal =
      ResolveUnpackedOrdinal(sv_index, left, right, data_.size());
  if (ordinal) {
    result.data_[*ordinal] = std::move(value);
  }
  return result;
}

auto RuntimeUnpackedArray::FromString(
    const String& text, const PackedType& element_type,
    const PackedArray& count) -> RuntimeUnpackedArray {
  const std::string_view chars = text.View();
  const auto element_count = static_cast<std::size_t>(count.ToInt64());
  const PackedArray element_default{element_type};
  std::vector<RuntimeValue> elements;
  elements.reserve(element_count);
  for (std::size_t i = 0; i < element_count; ++i) {
    elements.push_back(
        RuntimeValue{
            i < chars.size()
                ? PackedArray::FromInt(
                      static_cast<unsigned char>(chars[i]), element_type)
                : element_default});
  }
  RuntimeUnpackedArray result;
  result.element_default_ =
      std::make_unique<RuntimeValue>(RuntimeValue{element_default});
  result.data_ = std::move(elements);
  return result;
}

auto RuntimeUnpackedArray::FromPackedArray(
    const PackedArray& bits, const PackedType& element_type,
    const PackedArray& count) -> RuntimeUnpackedArray {
  const std::string bytes = bits.ByteString();
  const auto element_count = static_cast<std::size_t>(count.ToInt64());
  const PackedArray element_default{element_type};
  std::vector<RuntimeValue> elements;
  elements.reserve(element_count);
  for (std::size_t i = 0; i < element_count; ++i) {
    elements.push_back(
        RuntimeValue{
            i < bytes.size()
                ? PackedArray::FromInt(
                      static_cast<unsigned char>(bytes[i]), element_type)
                : element_default});
  }
  RuntimeUnpackedArray result;
  result.element_default_ =
      std::make_unique<RuntimeValue>(RuntimeValue{element_default});
  result.data_ = std::move(elements);
  return result;
}

auto RuntimeUnpackedArray::ToByteString() const -> String {
  std::string out;
  out.reserve(data_.size());
  for (const RuntimeValue& element : data_) {
    const auto* byte = std::get_if<PackedArray>(&element.value);
    if (byte == nullptr) {
      throw InternalError(
          "RuntimeUnpackedArray::ToByteString: a byte array holds packed "
          "elements");
    }
    out.push_back(static_cast<char>(byte->ToInt64() & 0xFF));
  }
  return String{std::move(out)};
}

auto RuntimeUnpackedArray::Slice(
    const PackedArray& a, const PackedArray& b, const PackedArray& form,
    const PackedArray& left, const PackedArray& right) const
    -> RuntimeUnpackedArray {
  const SliceWindow window = ResolveSliceWindow(a, b, form, left, right);
  RuntimeUnpackedArray result;
  result.element_default_ = std::make_unique<RuntimeValue>(*element_default_);
  result.data_ = detail::ArraySliceGather(
      data_, *element_default_, window.base, window.count, window.base_known);
  return result;
}

auto RuntimeUnpackedArray::WithSlice(
    const PackedArray& a, const PackedArray& b, const PackedArray& form,
    const PackedArray& left, const PackedArray& right,
    const RuntimeUnpackedArray& replacement) const -> RuntimeUnpackedArray {
  const SliceWindow window = ResolveSliceWindow(a, b, form, left, right);
  RuntimeUnpackedArray result(*this);
  detail::ArraySliceScatter(
      result.data_, window.base, window.count, replacement.data_,
      window.base_known);
  return result;
}

// A declared range is never empty, so the fold below always runs at least once
// for a value a source program can spell; seeding it with the identity keeps
// the compiler-internal empty array from indexing storage that is not there.
auto RuntimeUnpackedArray::operator==(const RuntimeUnpackedArray& other) const
    -> PackedArray {
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    result = result && RuntimeValueEqual(data_[i], other.data_[i]);
  }
  return result;
}

auto RuntimeUnpackedArray::operator!=(const RuntimeUnpackedArray& other) const
    -> PackedArray {
  return !(*this == other);
}

auto RuntimeUnpackedArray::CaseEqual(const RuntimeUnpackedArray& other) const
    -> PackedArray {
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    result = result && RuntimeValueCaseEqual(data_[i], other.data_[i]);
  }
  return result;
}

auto RuntimeUnpackedArray::MergeConditional(
    const RuntimeUnpackedArray& other) const -> RuntimeUnpackedArray {
  const bool paired = data_.size() == other.data_.size();
  RuntimeUnpackedArray result = *this;
  for (std::size_t i = 0; i < result.data_.size(); ++i) {
    const bool agree =
        paired && RuntimeValueEqual(data_[i], other.data_[i]).Truth() ==
                      Truthiness::kKnownNonzero;
    if (!agree) {
      result.data_[i] = *element_default_;
    }
  }
  return result;
}

auto RuntimeUnpackedArray::ResolveTriState(
    const RuntimeUnpackedArray& other) const -> RuntimeUnpackedArray {
  RuntimeUnpackedArray resolved = *this;
  for (std::size_t i = 0; i < resolved.data_.size(); ++i) {
    resolved.data_[i] = RuntimeValueResolveTriState(data_[i], other.data_[i]);
  }
  return resolved;
}

auto RuntimeUnpackedArray::HighImpedanceLike(
    const RuntimeUnpackedArray& prototype) -> RuntimeUnpackedArray {
  RuntimeUnpackedArray floating = prototype;
  for (RuntimeValue& element : floating.data_) {
    element = RuntimeValueHighImpedanceLike(element);
  }
  return floating;
}

// LRM 9.4.2: a size mismatch is a change. That is how the empty default of a
// fresh cell is told apart from the first sized write, so the declared-shape
// initializer raises an update event.
auto RuntimeUnpackedArray::IsBitIdentical(
    const RuntimeUnpackedArray& other) const -> bool {
  if (data_.size() != other.data_.size()) {
    return false;
  }
  for (std::size_t i = 0; i < data_.size(); ++i) {
    if (!RuntimeValueBitIdentical(data_[i], other.data_[i])) {
      return false;
    }
  }
  return true;
}

auto RuntimeUnpackedArray::HasUnknown() const -> bool {
  return std::ranges::any_of(data_, [](const RuntimeValue& element) {
    return RuntimeValueHasUnknown(element);
  });
}

auto RuntimeUnpackedArray::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeUnpackedArray::BitstreamWidth() const -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeValue& element : data_) {
    total = total + RuntimeValueBitstreamWidth(element);
  }
  return total;
}

auto RuntimeUnpackedArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeValue& element : data_) {
    total = total + RuntimeValueCountBits(element, control_bits);
  }
  return total;
}

}  // namespace lyra::value
