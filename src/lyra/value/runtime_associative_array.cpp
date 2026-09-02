#include "lyra/value/runtime_associative_array.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::value {

struct RuntimeAssociativeEntry {
  RuntimeValue index;
  RuntimeValue element;
};

namespace {

// Two indices name one entry when neither orders before the other, so the
// ordering is the whole of what tells one index from another.
auto SameIndex(const RuntimeValue& a, const RuntimeValue& b) -> bool {
  return !RuntimeValueOrderBefore(a, b) && !RuntimeValueOrderBefore(b, a);
}

}  // namespace

RuntimeAssociativeArray::RuntimeAssociativeArray()
    : element_default_(std::make_unique<RuntimeValue>()) {
}

RuntimeAssociativeArray::RuntimeAssociativeArray(RuntimeValue element_default)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))) {
}

RuntimeAssociativeArray::RuntimeAssociativeArray(
    RuntimeValue element_default, RuntimeValue user_default)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))),
      user_default_(std::make_unique<RuntimeValue>(std::move(user_default))) {
}

RuntimeAssociativeArray::RuntimeAssociativeArray(
    const RuntimeAssociativeArray& other)
    : element_default_(std::make_unique<RuntimeValue>(*other.element_default_)),
      user_default_(
          other.user_default_ == nullptr
              ? nullptr
              : std::make_unique<RuntimeValue>(*other.user_default_)),
      data_(other.data_) {
}

RuntimeAssociativeArray::RuntimeAssociativeArray(
    RuntimeAssociativeArray&&) noexcept = default;

auto RuntimeAssociativeArray::operator=(const RuntimeAssociativeArray& other)
    -> RuntimeAssociativeArray& {
  if (this != &other) {
    element_default_ = std::make_unique<RuntimeValue>(*other.element_default_);
    user_default_ = other.user_default_ == nullptr
                        ? nullptr
                        : std::make_unique<RuntimeValue>(*other.user_default_);
    data_ = other.data_;
  }
  return *this;
}

auto RuntimeAssociativeArray::operator=(RuntimeAssociativeArray&&) noexcept
    -> RuntimeAssociativeArray& = default;

RuntimeAssociativeArray::~RuntimeAssociativeArray() = default;

auto RuntimeAssociativeArray::LowerBound(const RuntimeValue& index) const
    -> std::size_t {
  const auto position = std::ranges::lower_bound(
      data_, index, RuntimeValueOrderBefore,
      [](const RuntimeAssociativeEntry& entry) -> const RuntimeValue& {
        return entry.index;
      });
  return static_cast<std::size_t>(
      std::ranges::distance(data_.begin(), position));
}

auto RuntimeAssociativeArray::Find(const RuntimeValue& index) const
    -> std::optional<std::size_t> {
  if (RuntimeValueHasUnknown(index)) {
    return std::nullopt;
  }
  const std::size_t position = LowerBound(index);
  if (position < data_.size() && SameIndex(index, data_[position].index)) {
    return position;
  }
  return std::nullopt;
}

auto RuntimeAssociativeArray::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(data_.size()));
}

auto RuntimeAssociativeArray::ElementDefault() const -> const RuntimeValue& {
  return *element_default_;
}

auto RuntimeAssociativeArray::Exists(const RuntimeValue& index) const
    -> PackedArray {
  return PackedArray::Int(Find(index).has_value() ? 1 : 0);
}

auto RuntimeAssociativeArray::Element(const RuntimeValue& index) const
    -> const RuntimeValue& {
  if (const std::optional<std::size_t> position = Find(index)) {
    return data_[*position].element;
  }
  return user_default_ == nullptr ? *element_default_ : *user_default_;
}

auto RuntimeAssociativeArray::IndexAt(std::size_t position) const
    -> const RuntimeValue& {
  if (position >= data_.size()) {
    throw InternalError(
        "RuntimeAssociativeArray::IndexAt: the position is past the last");
  }
  return data_[position].index;
}

auto RuntimeAssociativeArray::ElementAt(std::size_t position) const
    -> const RuntimeValue& {
  if (position >= data_.size()) {
    throw InternalError(
        "RuntimeAssociativeArray::ElementAt: the position is past the last");
  }
  return data_[position].element;
}

auto RuntimeAssociativeArray::WithElement(
    const RuntimeValue& index, RuntimeValue value) const
    -> RuntimeAssociativeArray {
  RuntimeAssociativeArray result(*this);
  if (const std::optional<std::size_t> position = result.Find(index)) {
    result.data_[*position].element = std::move(value);
    return result;
  }
  if (RuntimeValueHasUnknown(index)) {
    return result;
  }
  result.data_.insert(
      result.data_.begin() +
          static_cast<std::ptrdiff_t>(result.LowerBound(index)),
      RuntimeAssociativeEntry{.index = index, .element = std::move(value)});
  return result;
}

auto RuntimeAssociativeArray::Delete() const -> RuntimeAssociativeArray {
  RuntimeAssociativeArray result(*this);
  result.data_.clear();
  return result;
}

auto RuntimeAssociativeArray::Delete(const RuntimeValue& index) const
    -> RuntimeAssociativeArray {
  RuntimeAssociativeArray result(*this);
  if (const std::optional<std::size_t> position = result.Find(index)) {
    result.data_.erase(
        result.data_.begin() + static_cast<std::ptrdiff_t>(*position));
  }
  return result;
}

auto RuntimeAssociativeArray::FirstIndex() const
    -> std::optional<RuntimeValue> {
  if (data_.empty()) {
    return std::nullopt;
  }
  return data_.front().index;
}

auto RuntimeAssociativeArray::LastIndex() const -> std::optional<RuntimeValue> {
  if (data_.empty()) {
    return std::nullopt;
  }
  return data_.back().index;
}

auto RuntimeAssociativeArray::NextIndex(const RuntimeValue& probe) const
    -> std::optional<RuntimeValue> {
  std::size_t position = LowerBound(probe);
  if (position < data_.size() && SameIndex(probe, data_[position].index)) {
    ++position;
  }
  if (position >= data_.size()) {
    return std::nullopt;
  }
  return data_[position].index;
}

auto RuntimeAssociativeArray::PrevIndex(const RuntimeValue& probe) const
    -> std::optional<RuntimeValue> {
  const std::size_t position = LowerBound(probe);
  if (position == 0) {
    return std::nullopt;
  }
  return data_[position - 1].index;
}

auto RuntimeAssociativeArray::operator==(
    const RuntimeAssociativeArray& other) const -> PackedArray {
  if (data_.size() != other.data_.size()) {
    return PackedArray::Bit(false);
  }
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    if (!SameIndex(data_[i].index, other.data_[i].index)) {
      return PackedArray::Bit(false);
    }
    result =
        result && RuntimeValueEqual(data_[i].element, other.data_[i].element);
  }
  return result;
}

auto RuntimeAssociativeArray::operator!=(
    const RuntimeAssociativeArray& other) const -> PackedArray {
  return !(*this == other);
}

auto RuntimeAssociativeArray::CaseEqual(
    const RuntimeAssociativeArray& other) const -> PackedArray {
  if (data_.size() != other.data_.size()) {
    return PackedArray::Bit(false);
  }
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    if (!SameIndex(data_[i].index, other.data_[i].index)) {
      return PackedArray::Bit(false);
    }
    result = result &&
             RuntimeValueCaseEqual(data_[i].element, other.data_[i].element);
  }
  return result;
}

auto RuntimeAssociativeArray::IsBitIdentical(
    const RuntimeAssociativeArray& other) const -> bool {
  if (data_.size() != other.data_.size()) {
    return false;
  }
  for (std::size_t i = 0; i < data_.size(); ++i) {
    if (!SameIndex(data_[i].index, other.data_[i].index)) {
      return false;
    }
    if (!RuntimeValueBitIdentical(data_[i].element, other.data_[i].element)) {
      return false;
    }
  }
  return true;
}

auto RuntimeAssociativeArray::HasUnknown() const -> bool {
  return std::ranges::any_of(data_, [](const RuntimeAssociativeEntry& entry) {
    return RuntimeValueHasUnknown(entry.element);
  });
}

auto RuntimeAssociativeArray::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeAssociativeArray::BitstreamWidth() const -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeAssociativeEntry& entry : data_) {
    total = total + RuntimeValueBitstreamWidth(entry.element);
  }
  return total;
}

auto RuntimeAssociativeArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeAssociativeEntry& entry : data_) {
    total = total + RuntimeValueCountBits(entry.element, control_bits);
  }
  return total;
}

}  // namespace lyra::value
