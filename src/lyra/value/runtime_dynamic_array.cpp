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

RuntimeDynamicArray::RuntimeDynamicArray(const value::ElementType& element)
    : core_(WitnessedElem(element)) {
}

RuntimeDynamicArray::RuntimeDynamicArray(
    const value::ElementType& element, std::size_t count,
    const RuntimeDynamicArray* from)
    : core_(
          WitnessedElem(element), count,
          from == nullptr ? nullptr : &from->core_) {
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

auto RuntimeDynamicArray::FromElements(
    const value::ElementType& element, std::span<const void* const> items)
    -> RuntimeDynamicArray {
  return RuntimeDynamicArray(
      BasicDynamicArray<WitnessedElem>(WitnessedElem(element)).Extended(items));
}

auto RuntimeDynamicArray::ElementType() const -> const value::ElementType& {
  return core_.Element().ElementType();
}

auto RuntimeDynamicArray::Count() const -> std::size_t {
  return core_.Count();
}

auto RuntimeDynamicArray::Size() const -> Int {
  return Int::FromInt(static_cast<std::int64_t>(Count()));
}

auto RuntimeDynamicArray::ElementAt(std::size_t position) const -> const void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeDynamicArray::ElementAt: the position is past the last");
  }
  return core_.At(position);
}

auto RuntimeDynamicArray::ElementAt(std::size_t position) -> void* {
  if (position >= Count()) {
    throw InternalError(
        "RuntimeDynamicArray::ElementAt: the position is past the last");
  }
  return core_.At(position);
}

auto RuntimeDynamicArray::Element(std::optional<std::int64_t> position) const
    -> const void* {
  return core_.ElementAt(position);
}

auto RuntimeDynamicArray::ElementMut(
    std::optional<std::int64_t> position, Formation& formed) -> void* {
  return core_.ElementMut(position, formed);
}

auto RuntimeDynamicArray::Lease(void* element) const -> ContainerLease {
  return core_.Lease(element);
}

void RuntimeDynamicArray::Delete() {
  core_.Delete();
}

auto RuntimeDynamicArray::SliceElements(
    std::optional<std::int64_t> start, std::int64_t count) const
    -> std::vector<const void*> {
  return core_.SliceElements(start, SliceCount(count));
}

auto RuntimeDynamicArray::AssignSlice(
    std::optional<std::int64_t> start, std::int64_t count,
    std::span<const void* const> replacement) -> bool {
  return core_.AssignSlice(start, SliceCount(count), replacement);
}

void RuntimeDynamicArray::Permute(std::span<const std::size_t> order) {
  core_.Permute(order);
}

auto RuntimeDynamicArray::Equal(
    const RuntimeDynamicArray& other, const ValueOperations& element) const
    -> FourStateBit {
  return detail::SequenceEqual(
      core_, other.core_, WitnessedOperations(element));
}

auto RuntimeDynamicArray::CaseEqual(
    const RuntimeDynamicArray& other, const ValueOperations& element) const
    -> Bit {
  return detail::SequenceCaseEqual(
      core_, other.core_, WitnessedOperations(element));
}

auto RuntimeDynamicArray::MergeConditional(
    const RuntimeDynamicArray& other, const ValueOperations& element) const
    -> RuntimeDynamicArray {
  return RuntimeDynamicArray(
      core_.MergeConditional(other.core_, WitnessedOperations(element)));
}

auto RuntimeDynamicArray::IsBitIdentical(const RuntimeDynamicArray& other) const
    -> bool {
  return detail::SequenceBitIdentical(core_, other.core_);
}

auto RuntimeDynamicArray::HasUnknown(const ValueOperations& element) const
    -> bool {
  return detail::SequenceHasUnknown(core_, WitnessedOperations(element));
}

auto RuntimeDynamicArray::BitstreamWidth(const ValueOperations& element) const
    -> Int {
  return Int::FromInt(
      detail::SequenceBitstreamWidth(core_, WitnessedOperations(element)));
}

auto RuntimeDynamicArray::CountBits(
    const LogicVector<4>& control_bits, const ValueOperations& element) const
    -> Int {
  return Int::FromInt(
      detail::SequenceCountBits(
          core_, control_bits, WitnessedOperations(element)));
}

auto RuntimeDynamicArray::WriteToStream(
    const IntegralView& stream, std::uint64_t filled,
    const ValueOperations& element) const -> std::uint64_t {
  return core_.WriteToStream(stream, filled, WitnessedOperations(element));
}

auto RuntimeDynamicArray::ReadFromStream(
    const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
    const ValueOperations& element) -> std::uint64_t {
  return core_.ReadFromStream(
      stream, taken, until, WitnessedOperations(element));
}

auto RuntimeDynamicArray::ToBitVec(const ValueOperations& element) const
    -> BitVec {
  return StreamOf(*this, BitstreamWidth(element), element);
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
  return core_.WindowToBitVec(start, count, WitnessedOperations(element));
}

auto RuntimeDynamicArray::WithWindow(
    std::optional<std::int64_t> start, std::size_t count,
    std::span<const void* const> values) const -> RuntimeDynamicArray {
  return RuntimeDynamicArray(core_.WithWindow(start, count, values));
}

}  // namespace lyra::value
