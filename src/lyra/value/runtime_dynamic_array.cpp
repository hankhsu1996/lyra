#include "lyra/value/runtime_dynamic_array.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

RuntimeDynamicArray::RuntimeDynamicArray() = default;

RuntimeDynamicArray::RuntimeDynamicArray(
    const value::ElementType& element, std::size_t count,
    const RuntimeDynamicArray* from)
    : core_(
          std::in_place, WitnessedElem(element), count,
          from == nullptr ? nullptr : &from->Core()) {
}

RuntimeDynamicArray::RuntimeDynamicArray(BasicDynamicArray<WitnessedElem> core)
    : core_(std::move(core)) {
}

RuntimeDynamicArray::RuntimeDynamicArray(const RuntimeDynamicArray&) = default;
RuntimeDynamicArray::RuntimeDynamicArray(RuntimeDynamicArray&&) noexcept =
    default;
auto RuntimeDynamicArray::operator=(const RuntimeDynamicArray&)
    -> RuntimeDynamicArray& = default;
auto RuntimeDynamicArray::operator=(RuntimeDynamicArray&&) noexcept
    -> RuntimeDynamicArray& = default;
RuntimeDynamicArray::~RuntimeDynamicArray() = default;

// An array whose declaration has not installed its element type has no
// elements to act on, so being asked to act on one is a lowering defect.
void RuntimeDynamicArray::RequireInstalled() const {
  if (!core_.has_value()) {
    throw InternalError(
        "RuntimeDynamicArray: an array is used before its declaration "
        "installs its element type -- please report this as a bug");
  }
}

auto RuntimeDynamicArray::Core() const
    -> const BasicDynamicArray<WitnessedElem>& {
  RequireInstalled();
  return *core_;
}

auto RuntimeDynamicArray::Core() -> BasicDynamicArray<WitnessedElem>& {
  RequireInstalled();
  return *core_;
}

auto RuntimeDynamicArray::FromElements(
    const value::ElementType& element, std::span<const void* const> items)
    -> RuntimeDynamicArray {
  return RuntimeDynamicArray(
      BasicDynamicArray<WitnessedElem>(WitnessedElem(element)).Extended(items));
}

auto RuntimeDynamicArray::ElementType() const -> const value::ElementType& {
  return Core().Element().ElementType();
}

auto RuntimeDynamicArray::Count() const -> std::size_t {
  return core_.has_value() ? core_->Count() : 0;
}

auto RuntimeDynamicArray::Size() const -> Int {
  return Int::FromInt(static_cast<std::int64_t>(Count()));
}

auto RuntimeDynamicArray::ElementAt(std::size_t position) const -> const void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeDynamicArray::ElementAt: the position is past the last");
  }
  return Core().At(position);
}

auto RuntimeDynamicArray::ElementAt(std::size_t position) -> void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeDynamicArray::ElementAt: the position is past the last");
  }
  return Core().At(position);
}

auto RuntimeDynamicArray::Element(std::optional<std::int64_t> position) const
    -> const void* {
  return Core().ElementAt(position);
}

auto RuntimeDynamicArray::ElementMut(
    std::optional<std::int64_t> position, Formation& formed) -> void* {
  return Core().ElementMut(position, formed);
}

auto RuntimeDynamicArray::Lease(void* element) const -> ContainerLease {
  return Core().Lease(element);
}

void RuntimeDynamicArray::Delete() {
  Core().Delete();
}

auto RuntimeDynamicArray::SliceElements(
    std::optional<std::int64_t> start, std::int64_t count) const
    -> std::vector<const void*> {
  return Core().SliceElements(start, SliceCount(count));
}

auto RuntimeDynamicArray::AssignSlice(
    std::optional<std::int64_t> start, std::int64_t count,
    std::span<const void* const> replacement) -> bool {
  return Core().AssignSlice(start, SliceCount(count), replacement);
}

void RuntimeDynamicArray::Permute(std::span<const std::size_t> order) {
  Core().Permute(order);
}

// An array with no element type yet holds no elements, which is all a
// comparison with one can read.
auto RuntimeDynamicArray::Equal(
    const RuntimeDynamicArray& other, const ValueOperations& element) const
    -> FourStateBit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return detail::ScalarOf(Count() == other.Count());
  }
  return detail::SequenceEqual(
      *core_, *other.core_, WitnessedOperations(element));
}

auto RuntimeDynamicArray::CaseEqual(
    const RuntimeDynamicArray& other, const ValueOperations& element) const
    -> Bit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Bit::FromBool(Count() == other.Count());
  }
  return detail::SequenceCaseEqual(
      *core_, *other.core_, WitnessedOperations(element));
}

auto RuntimeDynamicArray::MergeConditional(
    const RuntimeDynamicArray& other, const ValueOperations& element) const
    -> RuntimeDynamicArray {
  return RuntimeDynamicArray(
      Core().MergeConditional(other.Core(), WitnessedOperations(element)));
}

auto RuntimeDynamicArray::IsBitIdentical(const RuntimeDynamicArray& other) const
    -> bool {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Count() == other.Count();
  }
  return detail::SequenceBitIdentical(*core_, *other.core_);
}

auto RuntimeDynamicArray::HasUnknown(const ValueOperations& element) const
    -> bool {
  return core_.has_value() &&
         detail::SequenceHasUnknown(*core_, WitnessedOperations(element));
}

auto RuntimeDynamicArray::BitstreamWidth(const ValueOperations& element) const
    -> Int {
  return Int::FromInt(
      core_.has_value()
          ? detail::SequenceBitstreamWidth(*core_, WitnessedOperations(element))
          : 0);
}

auto RuntimeDynamicArray::CountBits(
    const ConstIntegralView& control_bits, const ValueOperations& element) const
    -> Int {
  return Int::FromInt(
      core_.has_value()
          ? detail::SequenceCountBits(
                *core_, control_bits, WitnessedOperations(element))
          : 0);
}

auto RuntimeDynamicArray::WriteToStream(
    const IntegralView& stream, std::uint64_t filled,
    const ValueOperations& element) const -> std::uint64_t {
  return core_.has_value() ? core_->WriteToStream(
                                 stream, filled, WitnessedOperations(element))
                           : filled;
}

auto RuntimeDynamicArray::ReadFromStream(
    const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
    const ValueOperations& element) -> std::uint64_t {
  return Core().ReadFromStream(
      stream, taken, until, WitnessedOperations(element));
}

auto RuntimeDynamicArray::ToBitVec(const ValueOperations& element) const
    -> BitVec {
  return StreamOf(*this, element);
}

auto RuntimeDynamicArray::FromBitVec(
    const value::ElementType& element, const BitVec& run,
    const ValueOperations& operations) -> RuntimeDynamicArray {
  RuntimeDynamicArray out = FromElements(element, {});
  ReadWholeStream(out, run, operations);
  return out;
}

auto RuntimeDynamicArray::WindowToBitVec(
    std::optional<std::int64_t> start, std::size_t count,
    const ValueOperations& element) const -> BitVec {
  return Core().WindowToBitVec(start, count, WitnessedOperations(element));
}

auto RuntimeDynamicArray::WithWindow(
    std::optional<std::int64_t> start, std::size_t count,
    std::span<const void* const> values) const -> RuntimeDynamicArray {
  return RuntimeDynamicArray(Core().WithWindow(start, count, values));
}

}  // namespace lyra::value
