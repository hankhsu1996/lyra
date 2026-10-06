#include "lyra/value/runtime_dynamic_array.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/value_type.hpp"

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

auto RuntimeDynamicArray::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(Count()));
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

auto RuntimeDynamicArray::Element(const PackedArray& position) const -> const
    void* {
  return Core().ElementAt(position);
}

auto RuntimeDynamicArray::ElementRef(
    const PackedArray& position, Formation& formed) -> void* {
  return Core().ElementRef(position, formed);
}

void RuntimeDynamicArray::Delete() {
  Core().Delete();
}

auto RuntimeDynamicArray::SliceElements(
    const PackedArray& start, std::int64_t count) const
    -> std::vector<const void*> {
  return Core().SliceElements(ReadPosition(start), SliceCount(count));
}

auto RuntimeDynamicArray::AssignSlice(
    const PackedArray& start, std::int64_t count,
    std::span<const void* const> replacement) -> bool {
  return Core().AssignSlice(
      ReadPosition(start), SliceCount(count), replacement);
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
    -> PackedArray {
  if (!core_.has_value() || !other.core_.has_value()) {
    return PackedArray::Bit(Count() == other.Count());
  }
  return detail::SequenceEqual(*core_, *other.core_);
}

auto RuntimeDynamicArray::operator!=(const RuntimeDynamicArray& other) const
    -> PackedArray {
  return !(*this == other);
}

auto RuntimeDynamicArray::CaseEqual(const RuntimeDynamicArray& other) const
    -> PackedArray {
  if (!core_.has_value() || !other.core_.has_value()) {
    return PackedArray::Bit(Count() == other.Count());
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

auto RuntimeDynamicArray::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeDynamicArray::BitstreamWidth() const -> PackedArray {
  return core_.has_value() ? detail::SequenceBitstreamWidth(*core_)
                           : PackedArray::Int(0);
}

auto RuntimeDynamicArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  return core_.has_value() ? detail::SequenceCountBits(*core_, control_bits)
                           : PackedArray::Int(0);
}

}  // namespace lyra::value
