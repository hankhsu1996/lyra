#include "lyra/value/runtime_dynamic_array.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/runtime_value.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

RuntimeDynamicArray::RuntimeDynamicArray()
    : element_default_(std::make_unique<RuntimeValue>()) {
}

RuntimeDynamicArray::RuntimeDynamicArray(RuntimeValue element_default)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))) {
}

RuntimeDynamicArray::RuntimeDynamicArray(
    const PackedArray& n, RuntimeValue element_default)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))) {
  const std::int64_t count = n.ToInt64();
  if (count < 0) {
    throw SimulationError(
        "dynamic array new[N]: size operand is negative (LRM 7.5.1)");
  }
  data_.assign(static_cast<std::size_t>(count), *element_default_);
}

RuntimeDynamicArray::RuntimeDynamicArray(
    const PackedArray& n, RuntimeValue element_default,
    const RuntimeDynamicArray& src)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))),
      data_(src.data_) {
  const std::int64_t count = n.ToInt64();
  if (count < 0) {
    throw SimulationError(
        "dynamic array new[N](src): size operand is negative (LRM 7.5.1)");
  }
  data_.resize(static_cast<std::size_t>(count), *element_default_);
}

RuntimeDynamicArray::RuntimeDynamicArray(
    RuntimeValue element_default, std::vector<RuntimeValue> elements)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))),
      data_(std::move(elements)) {
}

RuntimeDynamicArray::RuntimeDynamicArray(const RuntimeDynamicArray& other)
    : element_default_(std::make_unique<RuntimeValue>(*other.element_default_)),
      data_(other.data_) {
}

RuntimeDynamicArray::RuntimeDynamicArray(RuntimeDynamicArray&&) noexcept =
    default;

auto RuntimeDynamicArray::operator=(const RuntimeDynamicArray& other)
    -> RuntimeDynamicArray& {
  if (this != &other) {
    element_default_ = std::make_unique<RuntimeValue>(*other.element_default_);
    data_ = other.data_;
  }
  return *this;
}

auto RuntimeDynamicArray::operator=(RuntimeDynamicArray&&) noexcept
    -> RuntimeDynamicArray& = default;

RuntimeDynamicArray::~RuntimeDynamicArray() = default;

auto RuntimeDynamicArray::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(data_.size()));
}

auto RuntimeDynamicArray::ElementDefault() const -> const RuntimeValue& {
  return *element_default_;
}

auto RuntimeDynamicArray::Element(const PackedArray& position) const
    -> const RuntimeValue& {
  const std::optional<std::size_t> ordinal =
      ElementOrdinal(position, data_.size());
  if (!ordinal) {
    return *element_default_;
  }
  return data_[*ordinal];
}

auto RuntimeDynamicArray::ElementAt(std::size_t position) const
    -> const RuntimeValue& {
  if (position >= data_.size()) {
    throw InternalError(
        "RuntimeDynamicArray::ElementAt: the position is past the last");
  }
  return data_[position];
}

auto RuntimeDynamicArray::ElementRef(const PackedArray& position)
    -> RuntimeValue& {
  const std::optional<std::size_t> ordinal =
      ElementOrdinal(position, data_.size());
  if (!ordinal) {
    return DiscardTarget(*element_default_);
  }
  return data_[*ordinal];
}

void RuntimeDynamicArray::Delete() {
  data_.clear();
}

auto RuntimeDynamicArray::Slice(const PackedArray& start, std::int64_t count)
    const -> RuntimeUnpackedArray {
  return RuntimeUnpackedArray::FromValues(
      *element_default_,
      detail::ArraySliceGather(
          data_, *element_default_, ReadPosition(start), SliceCount(count)));
}

void RuntimeDynamicArray::AssignSlice(
    const PackedArray& start, std::int64_t count,
    const RuntimeUnpackedArray& replacement) {
  const std::size_t window = SliceCount(count);
  std::vector<RuntimeValue> replacement_values;
  replacement_values.reserve(window);
  for (std::size_t i = 0; i < window; ++i) {
    replacement_values.push_back(replacement.ElementAt(i));
  }
  detail::ArraySliceScatter(
      data_, ReadPosition(start), window, replacement_values);
}

auto RuntimeDynamicArray::ConcatElement(RuntimeValue item) const
    -> RuntimeDynamicArray {
  RuntimeDynamicArray result(*this);
  result.data_.push_back(std::move(item));
  return result;
}

auto RuntimeDynamicArray::ConcatSpread(const RuntimeValue& part) const
    -> RuntimeDynamicArray {
  RuntimeDynamicArray result(*this);
  const std::size_t count = RuntimeValueContainerSize(part);
  for (std::size_t i = 0; i < count; ++i) {
    result.data_.push_back(RuntimeValueContainerElementAt(part, i));
  }
  return result;
}

auto RuntimeDynamicArray::FromArray(
    const RuntimeValue& source, RuntimeValue element_default)
    -> RuntimeDynamicArray {
  RuntimeDynamicArray result(std::move(element_default));
  const std::size_t count = RuntimeValueContainerSize(source);
  for (std::size_t i = 0; i < count; ++i) {
    result.data_.push_back(RuntimeValueContainerElementAt(source, i));
  }
  return result;
}

auto RuntimeDynamicArray::operator==(const RuntimeDynamicArray& other) const
    -> PackedArray {
  // LRM 11.4.5: the answer carries the state class an element's own equality
  // produces, because that is what a run of them reduces to. Reading the class
  // off the element shape leaves an empty array no case of its own, and leaves
  // a size mismatch answering in the same class as every other comparison.
  const bool four_state =
      RuntimeValueEqual(ElementDefault(), ElementDefault()).IsFourState();
  if (data_.size() != other.data_.size()) {
    return PackedArray::FromInt(0, 1, false, four_state);
  }
  PackedArray result = PackedArray::FromInt(1, 1, false, four_state);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    result = result && RuntimeValueEqual(data_[i], other.data_[i]);
  }
  return result;
}

auto RuntimeDynamicArray::operator!=(const RuntimeDynamicArray& other) const
    -> PackedArray {
  return !(*this == other);
}

auto RuntimeDynamicArray::CaseEqual(const RuntimeDynamicArray& other) const
    -> PackedArray {
  if (data_.size() != other.data_.size()) {
    return PackedArray::Bit(false);
  }
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    result = result && RuntimeValueCaseEqual(data_[i], other.data_[i]);
  }
  return result;
}

auto RuntimeDynamicArray::IsBitIdentical(const RuntimeDynamicArray& other) const
    -> bool {
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

auto RuntimeDynamicArray::HasUnknown() const -> bool {
  return std::ranges::any_of(data_, [](const RuntimeValue& element) {
    return RuntimeValueHasUnknown(element);
  });
}

auto RuntimeDynamicArray::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeDynamicArray::BitstreamWidth() const -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeValue& element : data_) {
    total = total + RuntimeValueBitstreamWidth(element);
  }
  return total;
}

auto RuntimeDynamicArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeValue& element : data_) {
    total = total + RuntimeValueCountBits(element, control_bits);
  }
  return total;
}

}  // namespace lyra::value
