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
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

RuntimeDynamicArray::RuntimeDynamicArray() = default;

RuntimeDynamicArray::RuntimeDynamicArray(
    const ValueType& element, const void* element_default, std::size_t count,
    const RuntimeDynamicArray* from)
    : core_(
          std::in_place, WitnessedElem(element, element_default), count,
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
    const ValueType& element, const void* element_default,
    std::span<const void* const> items) -> RuntimeDynamicArray {
  return RuntimeDynamicArray(
      BasicDynamicArray<WitnessedElem>(WitnessedElem(element, element_default))
          .Extended(items));
}

auto RuntimeDynamicArray::ElementType() const -> const ValueType& {
  return Core().Element().Type();
}

auto RuntimeDynamicArray::ElementDefault() const -> const void* {
  return Core().Element().Default();
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

auto RuntimeDynamicArray::ElementRef(
    std::optional<std::int64_t> position, Formation& formed) -> void* {
  return Core().ElementRef(position, formed);
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

auto RuntimeDynamicArray::Concat(std::span<const void* const> items) const
    -> RuntimeDynamicArray {
  return RuntimeDynamicArray(Core().Extended(items));
}

void RuntimeDynamicArray::Permute(std::span<const std::size_t> order) {
  Core().Permute(order);
}

// An array with no element type yet holds no elements, which is all a
// comparison with one can read.
auto RuntimeDynamicArray::operator==(const RuntimeDynamicArray& other) const
    -> FourStateBit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return detail::ScalarOf(Count() == other.Count());
  }
  return detail::SequenceEqual(*core_, *other.core_);
}

auto RuntimeDynamicArray::operator!=(const RuntimeDynamicArray& other) const
    -> FourStateBit {
  return Inverted(*this == other);
}

auto RuntimeDynamicArray::CaseEqual(const RuntimeDynamicArray& other) const
    -> Bit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Bit::FromBool(Count() == other.Count());
  }
  return detail::SequenceCaseEqual(*core_, *other.core_);
}

auto RuntimeDynamicArray::IsBitIdentical(const RuntimeDynamicArray& other) const
    -> bool {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Count() == other.Count();
  }
  return detail::SequenceBitIdentical(*core_, *other.core_);
}

auto RuntimeDynamicArray::HasUnknown() const -> bool {
  return core_.has_value() && detail::SequenceHasUnknown(*core_);
}

auto RuntimeDynamicArray::IsUnknown() const -> Bit {
  return Bit::FromBool(HasUnknown());
}

auto RuntimeDynamicArray::BitstreamWidth() const -> Int {
  return Int::FromInt(
      core_.has_value() ? detail::SequenceBitstreamWidth(*core_) : 0);
}

auto RuntimeDynamicArray::CountBits(const ConstIntegralView& control_bits) const
    -> Int {
  return Int::FromInt(
      core_.has_value() ? detail::SequenceCountBits(*core_, control_bits) : 0);
}

}  // namespace lyra::value
