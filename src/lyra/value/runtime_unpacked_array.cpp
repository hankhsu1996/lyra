#include "lyra/value/runtime_unpacked_array.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

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
    const String& text, const PackedArray& element_prototype,
    const PackedArray& count) -> RuntimeUnpackedArray {
  const std::string_view chars = text.View();
  const auto element_count = static_cast<std::size_t>(count.ToInt64());
  std::vector<RuntimeValue> elements;
  elements.reserve(element_count);
  for (std::size_t i = 0; i < element_count; ++i) {
    const auto byte =
        i < chars.size() ? static_cast<unsigned char>(chars[i]) : 0U;
    elements.push_back(
        RuntimeValue{PackedArray::FromInt(
            static_cast<std::int64_t>(byte), element_prototype)});
  }
  RuntimeUnpackedArray result;
  result.element_default_ =
      std::make_unique<RuntimeValue>(RuntimeValue{element_prototype});
  result.data_ = std::move(elements);
  return result;
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
