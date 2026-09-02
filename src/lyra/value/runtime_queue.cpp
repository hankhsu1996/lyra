#include "lyra/value/runtime_queue.hpp"

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
#include "lyra/value/slice_selector.hpp"

namespace lyra::value {

namespace {

// A bound is the greatest index the queue may hold (LRM 7.10.5). A queue with
// no bound spells that as a negative one, so a bound and its absence reach
// every construction and every store as the same operand rather than as two
// argument lists.
auto BoundOf(const PackedArray& max_bound) -> std::optional<std::uint64_t> {
  const std::int64_t bound = max_bound.ToInt64();
  if (bound < 0) {
    return std::nullopt;
  }
  return static_cast<std::uint64_t>(bound);
}

}  // namespace

RuntimeQueue::RuntimeQueue()
    : element_default_(std::make_unique<RuntimeValue>()) {
}

RuntimeQueue::RuntimeQueue(RuntimeValue element_default)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))) {
}

RuntimeQueue::RuntimeQueue(
    RuntimeValue element_default, const PackedArray& max_bound)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))),
      max_bound_(BoundOf(max_bound)) {
}

RuntimeQueue::RuntimeQueue(
    RuntimeValue element_default, std::vector<RuntimeValue> elements)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))),
      data_(
          std::make_move_iterator(elements.begin()),
          std::make_move_iterator(elements.end())) {
}

RuntimeQueue::RuntimeQueue(
    RuntimeValue element_default, std::vector<RuntimeValue> elements,
    const PackedArray& max_bound)
    : element_default_(
          std::make_unique<RuntimeValue>(std::move(element_default))),
      data_(
          std::make_move_iterator(elements.begin()),
          std::make_move_iterator(elements.end())),
      max_bound_(BoundOf(max_bound)) {
  EnforceBound();
}

RuntimeQueue::RuntimeQueue(const RuntimeQueue& other)
    : element_default_(std::make_unique<RuntimeValue>(*other.element_default_)),
      data_(other.data_),
      max_bound_(other.max_bound_) {
}

RuntimeQueue::RuntimeQueue(RuntimeQueue&&) noexcept = default;

auto RuntimeQueue::operator=(const RuntimeQueue& other) -> RuntimeQueue& {
  if (this != &other) {
    element_default_ = std::make_unique<RuntimeValue>(*other.element_default_);
    data_ = other.data_;
    max_bound_ = other.max_bound_;
  }
  return *this;
}

auto RuntimeQueue::operator=(RuntimeQueue&&) noexcept
    -> RuntimeQueue& = default;

RuntimeQueue::~RuntimeQueue() = default;

void RuntimeQueue::EnforceBound() {
  if (!max_bound_.has_value()) {
    return;
  }
  const std::size_t limit = static_cast<std::size_t>(*max_bound_) + 1;
  if (data_.size() > limit) {
    data_.resize(limit);
  }
}

auto RuntimeQueue::ConformBound(const PackedArray& max_bound) const
    -> RuntimeQueue {
  RuntimeQueue result(*this);
  result.max_bound_ = BoundOf(max_bound);
  result.EnforceBound();
  return result;
}

auto RuntimeQueue::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(data_.size()));
}

auto RuntimeQueue::ElementDefault() const -> const RuntimeValue& {
  return *element_default_;
}

auto RuntimeQueue::IsInvalidIndex(const PackedArray& index) const -> bool {
  if (index.HasUnknown()) {
    return true;
  }
  const std::int64_t value = index.ToInt64();
  return value < 0 || static_cast<std::uint64_t>(value) >=
                          static_cast<std::uint64_t>(data_.size());
}

auto RuntimeQueue::Element(const PackedArray& index) const
    -> const RuntimeValue& {
  if (IsInvalidIndex(index)) {
    return *element_default_;
  }
  return data_[static_cast<std::size_t>(index.ToInt64())];
}

auto RuntimeQueue::ElementAt(std::size_t position) const
    -> const RuntimeValue& {
  if (position >= data_.size()) {
    throw InternalError(
        "RuntimeQueue::ElementAt: the position is past the last");
  }
  return data_[position];
}

auto RuntimeQueue::WithElement(
    const PackedArray& index, RuntimeValue value) const -> RuntimeQueue {
  RuntimeQueue result(*this);
  if (index.HasUnknown()) {
    return result;
  }
  const std::int64_t position = index.ToInt64();
  if (position < 0) {
    return result;
  }
  const auto slot = static_cast<std::uint64_t>(position);
  if (slot == result.data_.size()) {
    result.data_.push_back(std::move(value));
    result.EnforceBound();
    return result;
  }
  if (slot < result.data_.size()) {
    result.data_[static_cast<std::size_t>(slot)] = std::move(value);
  }
  return result;
}

auto RuntimeQueue::Slice(
    const PackedArray& anchor, const PackedArray& extent,
    const PackedArray& form) const -> RuntimeQueue {
  RuntimeQueue result(*element_default_);
  if (anchor.HasUnknown() || extent.HasUnknown() || data_.empty()) {
    return result;
  }
  const std::int64_t anchor_value = anchor.ToInt64();
  const std::int64_t extent_value = extent.ToInt64();
  std::int64_t low = anchor_value;
  std::int64_t high = extent_value;
  switch (static_cast<SliceForm>(form.ToInt64())) {
    case SliceForm::kIndexedUp:
      high = anchor_value + extent_value - 1;
      break;
    case SliceForm::kIndexedDown:
      low = anchor_value - extent_value + 1;
      high = anchor_value;
      break;
    case SliceForm::kConstant:
      break;
  }
  const std::int64_t first = std::max<std::int64_t>(low, 0);
  const auto last =
      std::min<std::int64_t>(high, static_cast<std::int64_t>(data_.size()) - 1);
  for (std::int64_t i = first; i <= last; ++i) {
    result.data_.push_back(data_[static_cast<std::size_t>(i)]);
  }
  return result;
}

auto RuntimeQueue::PushFront(RuntimeValue item) const -> RuntimeQueue {
  RuntimeQueue result(*this);
  result.data_.push_front(std::move(item));
  result.EnforceBound();
  return result;
}

auto RuntimeQueue::PushBack(RuntimeValue item) const -> RuntimeQueue {
  RuntimeQueue result(*this);
  result.data_.push_back(std::move(item));
  result.EnforceBound();
  return result;
}

auto RuntimeQueue::Front() const -> const RuntimeValue& {
  return data_.empty() ? *element_default_ : data_.front();
}

auto RuntimeQueue::Back() const -> const RuntimeValue& {
  return data_.empty() ? *element_default_ : data_.back();
}

auto RuntimeQueue::PopFront() const -> RuntimeQueue {
  RuntimeQueue result(*this);
  if (!result.data_.empty()) {
    result.data_.pop_front();
  }
  return result;
}

auto RuntimeQueue::PopBack() const -> RuntimeQueue {
  RuntimeQueue result(*this);
  if (!result.data_.empty()) {
    result.data_.pop_back();
  }
  return result;
}

auto RuntimeQueue::Insert(const PackedArray& index, RuntimeValue item) const
    -> RuntimeQueue {
  RuntimeQueue result(*this);
  if (index.HasUnknown()) {
    return result;
  }
  const std::int64_t position = index.ToInt64();
  if (position < 0 ||
      static_cast<std::uint64_t>(position) > result.data_.size()) {
    return result;
  }
  result.data_.insert(
      result.data_.begin() + static_cast<std::ptrdiff_t>(position),
      std::move(item));
  result.EnforceBound();
  return result;
}

auto RuntimeQueue::Delete() const -> RuntimeQueue {
  RuntimeQueue result(*this);
  result.data_.clear();
  return result;
}

auto RuntimeQueue::Delete(const PackedArray& index) const -> RuntimeQueue {
  RuntimeQueue result(*this);
  if (IsInvalidIndex(index)) {
    return result;
  }
  result.data_.erase(
      result.data_.begin() + static_cast<std::ptrdiff_t>(index.ToInt64()));
  return result;
}

auto RuntimeQueue::operator==(const RuntimeQueue& other) const -> PackedArray {
  if (data_.size() != other.data_.size()) {
    return PackedArray::Bit(false);
  }
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    result = result && RuntimeValueEqual(data_[i], other.data_[i]);
  }
  return result;
}

auto RuntimeQueue::operator!=(const RuntimeQueue& other) const -> PackedArray {
  return !(*this == other);
}

auto RuntimeQueue::CaseEqual(const RuntimeQueue& other) const -> PackedArray {
  if (data_.size() != other.data_.size()) {
    return PackedArray::Bit(false);
  }
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < data_.size(); ++i) {
    result = result && RuntimeValueCaseEqual(data_[i], other.data_[i]);
  }
  return result;
}

auto RuntimeQueue::IsBitIdentical(const RuntimeQueue& other) const -> bool {
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

auto RuntimeQueue::HasUnknown() const -> bool {
  return std::ranges::any_of(data_, [](const RuntimeValue& element) {
    return RuntimeValueHasUnknown(element);
  });
}

auto RuntimeQueue::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeQueue::BitstreamWidth() const -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeValue& element : data_) {
    total = total + RuntimeValueBitstreamWidth(element);
  }
  return total;
}

auto RuntimeQueue::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (const RuntimeValue& element : data_) {
    total = total + RuntimeValueCountBits(element, control_bits);
  }
  return total;
}

}  // namespace lyra::value
