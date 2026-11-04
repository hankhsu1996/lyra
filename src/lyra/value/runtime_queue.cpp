#include "lyra/value/runtime_queue.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

RuntimeQueue::RuntimeQueue(const QueueType& type)
    : core_(WitnessedElem(type.ElementType()), WitnessedBound(type)) {
}

RuntimeQueue::RuntimeQueue(Held core) : core_(std::move(core)) {
}

RuntimeQueue::RuntimeQueue(const RuntimeQueue&) = default;
RuntimeQueue::RuntimeQueue(RuntimeQueue&&) noexcept = default;
auto RuntimeQueue::operator=(const RuntimeQueue&) -> RuntimeQueue& = default;
auto RuntimeQueue::operator=(RuntimeQueue&&) noexcept
    -> RuntimeQueue& = default;
RuntimeQueue::~RuntimeQueue() = default;

auto RuntimeQueue::ElementType() const -> const value::ElementType& {
  return core_.Element().ElementType();
}

auto RuntimeQueue::Count() const -> std::size_t {
  return core_.Count();
}

auto RuntimeQueue::Size() const -> Int {
  return Int::FromInt(static_cast<std::int64_t>(Count()));
}

auto RuntimeQueue::ElementAt(std::size_t position) const -> const void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeQueue::ElementAt: the position is past the last");
  }
  return core_.At(position);
}

auto RuntimeQueue::ElementAt(std::size_t position) -> void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeQueue::ElementAt: the position is past the last");
  }
  return core_.At(position);
}

auto RuntimeQueue::Element(std::optional<std::int64_t> position) const -> const
    void* {
  return core_.ElementAt(position);
}

auto RuntimeQueue::ElementMut(
    std::optional<std::int64_t> position, Formation& formed) -> void* {
  return core_.ElementMut(position, formed);
}

auto RuntimeQueue::Lease(void* element) const -> ContainerLease {
  return core_.Lease(element);
}

auto RuntimeQueue::Slice(
    std::optional<std::int64_t> lo, std::optional<std::int64_t> hi,
    const QueueType& type) const -> RuntimeQueue {
  return RuntimeQueue(core_.Slice(lo, hi, WitnessedBound(type)));
}

auto RuntimeQueue::AssignSlice(
    std::optional<std::int64_t> lo, std::optional<std::int64_t> hi,
    std::span<const void* const> replacement) -> std::optional<SliceWindow> {
  return core_.AssignSlice(lo, hi, replacement);
}

void RuntimeQueue::PushFront(const void* item) {
  core_.PushFront(item);
}

void RuntimeQueue::PushBack(const void* item) {
  core_.PushBack(item);
}

void RuntimeQueue::Insert(std::optional<std::int64_t> index, const void* item) {
  core_.Insert(index, item);
}

auto RuntimeQueue::FromElements(
    const QueueType& type, std::span<const void* const> items) -> RuntimeQueue {
  RuntimeQueue result(type);
  result.core_.Assign(items);
  return result;
}

void RuntimeQueue::PopFront(void* out) {
  core_.PopFront(out);
}

void RuntimeQueue::PopBack(void* out) {
  core_.PopBack(out);
}

void RuntimeQueue::Delete() {
  core_.Delete();
}

void RuntimeQueue::DeleteIndex(std::optional<std::int64_t> index) {
  core_.DeleteIndex(index);
}

void RuntimeQueue::Permute(std::span<const std::size_t> order) {
  core_.Permute(order);
}

auto RuntimeQueue::Equal(
    const RuntimeQueue& other, const ValueOperations& element) const
    -> FourStateBit {
  return detail::SequenceEqual(
      core_, other.core_, WitnessedOperations(element));
}

auto RuntimeQueue::CaseEqual(
    const RuntimeQueue& other, const ValueOperations& element) const -> Bit {
  return detail::SequenceCaseEqual(
      core_, other.core_, WitnessedOperations(element));
}

auto RuntimeQueue::MergeConditional(
    const RuntimeQueue& other, const ValueOperations& element) const
    -> RuntimeQueue {
  return RuntimeQueue(
      core_.MergeConditional(other.core_, WitnessedOperations(element)));
}

auto RuntimeQueue::IsBitIdentical(const RuntimeQueue& other) const -> bool {
  return detail::SequenceBitIdentical(core_, other.core_);
}

auto RuntimeQueue::HasUnknown(const ValueOperations& element) const -> bool {
  return detail::SequenceHasUnknown(core_, WitnessedOperations(element));
}

auto RuntimeQueue::BitstreamWidth(const ValueOperations& element) const -> Int {
  return Int::FromInt(
      detail::SequenceBitstreamWidth(core_, WitnessedOperations(element)));
}

auto RuntimeQueue::CountBits(
    const LogicVector<4>& control_bits, const ValueOperations& element) const
    -> Int {
  return Int::FromInt(
      detail::SequenceCountBits(
          core_, control_bits, WitnessedOperations(element)));
}

auto RuntimeQueue::WriteToStream(
    const IntegralView& stream, std::uint64_t filled,
    const ValueOperations& element) const -> std::uint64_t {
  return core_.WriteToStream(stream, filled, WitnessedOperations(element));
}

auto RuntimeQueue::ReadFromStream(
    const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
    const ValueOperations& element) -> std::uint64_t {
  return core_.ReadFromStream(
      stream, taken, until, WitnessedOperations(element));
}

auto RuntimeQueue::ToBitVec(const ValueOperations& element) const -> BitVec {
  return StreamOf(*this, BitstreamWidth(element), element);
}

auto RuntimeQueue::FromBitVec(
    const QueueType& type, const BitVec& run, const ValueOperations& element)
    -> RuntimeQueue {
  RuntimeQueue out(type);
  ReadWholeStream(out, run, element);
  return out;
}

auto RuntimeQueue::WindowToBitVec(
    std::optional<std::int64_t> start, std::size_t count,
    const ValueOperations& element) const -> BitVec {
  return core_.WindowToBitVec(start, count, WitnessedOperations(element));
}

auto RuntimeQueue::WithWindow(
    std::optional<std::int64_t> start, std::size_t count,
    std::span<const void* const> values) const -> RuntimeQueue {
  return RuntimeQueue(core_.WithWindow(start, count, values));
}

}  // namespace lyra::value
