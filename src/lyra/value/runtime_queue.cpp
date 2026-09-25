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
#include "lyra/value/position.hpp"
#include "lyra/value/queue_bound.hpp"
#include "lyra/value/runtime_value.hpp"

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
    ReportBoundOverflow();
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

auto RuntimeQueue::Element(const PackedArray& position) const
    -> const RuntimeValue& {
  const std::optional<std::size_t> ordinal =
      ElementOrdinal(position, data_.size());
  if (!ordinal) {
    return *element_default_;
  }
  return data_[*ordinal];
}

auto RuntimeQueue::ElementAt(std::size_t position) const
    -> const RuntimeValue& {
  if (position >= data_.size()) {
    throw InternalError(
        "RuntimeQueue::ElementAt: the position is past the last");
  }
  return data_[position];
}

auto RuntimeQueue::ElementRef(const PackedArray& position) -> RuntimeValue& {
  if (const std::optional<std::size_t> ordinal =
          ElementOrdinal(position, data_.size())) {
    return data_[*ordinal];
  }
  const std::optional<std::int64_t> at = ReadPosition(position);
  if (at && static_cast<std::uint64_t>(*at) == data_.size()) {
    data_.push_back(*element_default_);
    EnforceBound();
    if (static_cast<std::uint64_t>(*at) < data_.size()) {
      return data_[static_cast<std::size_t>(*at)];
    }
  }
  return DiscardTarget(*element_default_);
}

auto RuntimeQueue::Slice(const PackedArray& lo, const PackedArray& hi) const
    -> RuntimeQueue {
  RuntimeQueue result(*element_default_);
  const std::optional<std::int64_t> low = ReadPosition(lo);
  const std::optional<std::int64_t> high = ReadPosition(hi);
  if (!low || !high) {
    return result;
  }
  const std::int64_t first = std::max<std::int64_t>(*low, 0);
  const auto last = std::min<std::int64_t>(
      *high, static_cast<std::int64_t>(data_.size()) - 1);
  for (std::int64_t i = first; i <= last; ++i) {
    result.data_.push_back(data_[static_cast<std::size_t>(i)]);
  }
  return result;
}

void RuntimeQueue::PushFront(RuntimeValue item) {
  data_.push_front(std::move(item));
  EnforceBound();
}

void RuntimeQueue::PushBack(RuntimeValue item) {
  data_.push_back(std::move(item));
  EnforceBound();
}

auto RuntimeQueue::ConcatSpread(const RuntimeValue& part) const
    -> RuntimeQueue {
  RuntimeQueue result(*this);
  const std::size_t count = RuntimeValueContainerSize(part);
  for (std::size_t i = 0; i < count; ++i) {
    result.data_.push_back(RuntimeValueContainerElementAt(part, i));
  }
  result.EnforceBound();
  return result;
}

auto RuntimeQueue::FromArray(
    const RuntimeValue& source, RuntimeValue element_default,
    const PackedArray& max_bound) -> RuntimeQueue {
  const std::int64_t bound = max_bound.ToInt64();
  RuntimeQueue result =
      bound >= 0 ? RuntimeQueue(std::move(element_default), max_bound)
                 : RuntimeQueue(std::move(element_default));
  const std::size_t count = RuntimeValueContainerSize(source);
  for (std::size_t i = 0; i < count; ++i) {
    result.data_.push_back(RuntimeValueContainerElementAt(source, i));
  }
  result.EnforceBound();
  return result;
}

auto RuntimeQueue::PopFront() -> RuntimeValue {
  if (data_.empty()) {
    return *element_default_;
  }
  RuntimeValue popped = std::move(data_.front());
  data_.pop_front();
  return popped;
}

auto RuntimeQueue::PopBack() -> RuntimeValue {
  if (data_.empty()) {
    return *element_default_;
  }
  RuntimeValue popped = std::move(data_.back());
  data_.pop_back();
  return popped;
}

void RuntimeQueue::Insert(const PackedArray& index, RuntimeValue item) {
  if (index.HasUnknown()) {
    return;
  }
  const std::int64_t position = index.ToInt64();
  if (position < 0 || static_cast<std::uint64_t>(position) > data_.size()) {
    return;
  }
  data_.insert(
      data_.begin() + static_cast<std::ptrdiff_t>(position), std::move(item));
  EnforceBound();
}

void RuntimeQueue::Delete() {
  data_.clear();
}

void RuntimeQueue::DeleteIndex(const PackedArray& index) {
  const std::optional<std::size_t> ordinal =
      ElementOrdinal(index, data_.size());
  if (!ordinal) {
    return;
  }
  data_.erase(data_.begin() + static_cast<std::ptrdiff_t>(*ordinal));
}

auto RuntimeQueue::operator==(const RuntimeQueue& other) const -> PackedArray {
  // LRM 11.4.5: the answer carries the state class an element's own equality
  // produces, because that is what a run of them reduces to. Reading the class
  // off the element shape leaves an empty queue no case of its own, and leaves
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
