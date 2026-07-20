#include "lyra/value/runtime_dynamic_array.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

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
    throw InternalError(
        "RuntimeDynamicArray::new[N]: size operand is negative (LRM 7.5.1)");
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
    throw InternalError(
        "RuntimeDynamicArray::new[N](src): size operand is negative "
        "(LRM 7.5.1)");
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

auto RuntimeDynamicArray::IsInvalidIndex(const PackedArray& index) const
    -> bool {
  if (index.HasUnknown()) {
    return true;
  }
  const std::int64_t value = index.ToInt64();
  return value < 0 || static_cast<std::uint64_t>(value) >=
                          static_cast<std::uint64_t>(data_.size());
}

auto RuntimeDynamicArray::Element(const PackedArray& index) const
    -> const RuntimeValue& {
  if (IsInvalidIndex(index)) {
    return *element_default_;
  }
  return data_[static_cast<std::size_t>(index.ToInt64())];
}

auto RuntimeDynamicArray::WithElement(
    const PackedArray& index, RuntimeValue value) const -> RuntimeDynamicArray {
  RuntimeDynamicArray result(*this);
  if (!IsInvalidIndex(index)) {
    result.data_[static_cast<std::size_t>(index.ToInt64())] = std::move(value);
  }
  return result;
}

auto RuntimeDynamicArray::Delete() const -> RuntimeDynamicArray {
  return RuntimeDynamicArray(*element_default_);
}

auto RuntimeDynamicArray::operator==(const RuntimeDynamicArray& other) const
    -> PackedArray {
  if (data_.size() != other.data_.size()) {
    return PackedArray::Bit(false);
  }
  PackedArray result = PackedArray::Bit(true);
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

}  // namespace lyra::value
