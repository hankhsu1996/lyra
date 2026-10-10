#include "lyra/value/runtime_queue.hpp"

#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/basic_queue.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

RuntimeQueue::RuntimeQueue() = default;

RuntimeQueue::RuntimeQueue(
    const ValueType& element, const void* element_default, std::int64_t bound)
    : core_(std::in_place, WitnessedElem(element, element_default)) {
  core_->SetBound(bound);
}

RuntimeQueue::RuntimeQueue(BasicQueue<WitnessedElem> core)
    : core_(std::move(core)) {
}

RuntimeQueue::RuntimeQueue(const RuntimeQueue&) = default;
RuntimeQueue::RuntimeQueue(RuntimeQueue&&) noexcept = default;
auto RuntimeQueue::operator=(const RuntimeQueue&) -> RuntimeQueue& = default;
auto RuntimeQueue::operator=(RuntimeQueue&&) noexcept
    -> RuntimeQueue& = default;
RuntimeQueue::~RuntimeQueue() = default;

// A queue whose declaration has not installed its element type has no
// elements to act on, so being asked to act on one is a lowering defect.
void RuntimeQueue::RequireInstalled() const {
  if (!core_.has_value()) {
    throw InternalError(
        "RuntimeQueue: a queue is used before its declaration installs its "
        "element type -- please report this as a bug");
  }
}

auto RuntimeQueue::Core() const -> const BasicQueue<WitnessedElem>& {
  RequireInstalled();
  return *core_;
}

auto RuntimeQueue::Core() -> BasicQueue<WitnessedElem>& {
  RequireInstalled();
  return *core_;
}

auto RuntimeQueue::ElementType() const -> const ValueType& {
  return Core().Element().Type();
}

auto RuntimeQueue::ElementDefault() const -> const void* {
  return Core().Element().Default();
}

auto RuntimeQueue::ConformBound(std::int64_t bound) const -> RuntimeQueue {
  return RuntimeQueue(Core().WithBound(bound));
}

auto RuntimeQueue::Count() const -> std::size_t {
  return core_.has_value() ? core_->Count() : 0;
}

auto RuntimeQueue::Size() const -> Int {
  return Int::FromInt(static_cast<std::int64_t>(Count()));
}

auto RuntimeQueue::ElementAt(std::size_t position) const -> const void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeQueue::ElementAt: the position is past the last");
  }
  return Core().At(position);
}

auto RuntimeQueue::ElementAt(std::size_t position) -> void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeQueue::ElementAt: the position is past the last");
  }
  return Core().At(position);
}

auto RuntimeQueue::Element(std::optional<std::int64_t> position) const -> const
    void* {
  return Core().ElementAt(position);
}

auto RuntimeQueue::ElementRef(
    std::optional<std::int64_t> position, Formation& formed) -> void* {
  return Core().ElementRef(position, formed);
}

auto RuntimeQueue::Slice(
    std::optional<std::int64_t> lo, std::optional<std::int64_t> hi) const
    -> RuntimeQueue {
  return RuntimeQueue(Core().Slice(lo, hi));
}

void RuntimeQueue::PushFront(const void* item) {
  Core().PushFront(item);
}

void RuntimeQueue::PushBack(const void* item) {
  Core().PushBack(item);
}

void RuntimeQueue::Insert(std::optional<std::int64_t> index, const void* item) {
  Core().Insert(index, item);
}

auto RuntimeQueue::Concat(std::span<const void* const> items) const
    -> RuntimeQueue {
  return RuntimeQueue(Core().Concat(items));
}

auto RuntimeQueue::FromElements(
    const ValueType& element, const void* element_default, std::int64_t bound,
    std::span<const void* const> items) -> RuntimeQueue {
  RuntimeQueue result(element, element_default, bound);
  result.Core().Assign(items);
  return result;
}

auto RuntimeQueue::FromElements(
    const ValueType& element, const void* element_default,
    std::span<const void* const> items) -> RuntimeQueue {
  RuntimeQueue result(
      BasicQueue<WitnessedElem>(WitnessedElem(element, element_default)));
  result.Core().Assign(items);
  return result;
}

void RuntimeQueue::PopFront(void* out) {
  Core().PopFront(out);
}

void RuntimeQueue::PopBack(void* out) {
  Core().PopBack(out);
}

void RuntimeQueue::Delete() {
  Core().Delete();
}

void RuntimeQueue::DeleteIndex(std::optional<std::int64_t> index) {
  Core().DeleteIndex(index);
}

void RuntimeQueue::Permute(std::span<const std::size_t> order) {
  Core().Permute(order);
}

// A queue with no element type yet holds no elements, which is all a
// comparison with one can read.
auto RuntimeQueue::operator==(const RuntimeQueue& other) const -> FourStateBit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return detail::ScalarOf(Count() == other.Count());
  }
  return detail::SequenceEqual(*core_, *other.core_);
}

auto RuntimeQueue::operator!=(const RuntimeQueue& other) const -> FourStateBit {
  return Inverted(*this == other);
}

auto RuntimeQueue::CaseEqual(const RuntimeQueue& other) const -> Bit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Bit::FromBool(Count() == other.Count());
  }
  return detail::SequenceCaseEqual(*core_, *other.core_);
}

auto RuntimeQueue::IsBitIdentical(const RuntimeQueue& other) const -> bool {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Count() == other.Count();
  }
  return detail::SequenceBitIdentical(*core_, *other.core_);
}

auto RuntimeQueue::HasUnknown() const -> bool {
  return core_.has_value() && detail::SequenceHasUnknown(*core_);
}

auto RuntimeQueue::IsUnknown() const -> Bit {
  return Bit::FromBool(HasUnknown());
}

auto RuntimeQueue::BitstreamWidth() const -> Int {
  return Int::FromInt(
      core_.has_value() ? detail::SequenceBitstreamWidth(*core_) : 0);
}

auto RuntimeQueue::CountBits(const ConstIntegralView& control_bits) const
    -> Int {
  return Int::FromInt(
      core_.has_value() ? detail::SequenceCountBits(*core_, control_bits) : 0);
}

}  // namespace lyra::value
