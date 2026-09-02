#include "lyra/value/runtime_tagged_union.hpp"

#include <cstddef>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::value {

RuntimeTaggedUnion::RuntimeTaggedUnion() : payload_(1) {
}
RuntimeTaggedUnion::RuntimeTaggedUnion(
    std::size_t tag_index, RuntimeValue payload)
    : tag_index_(tag_index) {
  payload_.push_back(std::move(payload));
}
RuntimeTaggedUnion::RuntimeTaggedUnion(const RuntimeTaggedUnion&) = default;
RuntimeTaggedUnion::RuntimeTaggedUnion(RuntimeTaggedUnion&&) noexcept = default;
auto RuntimeTaggedUnion::operator=(const RuntimeTaggedUnion&)
    -> RuntimeTaggedUnion& = default;
auto RuntimeTaggedUnion::operator=(RuntimeTaggedUnion&&) noexcept
    -> RuntimeTaggedUnion& = default;
RuntimeTaggedUnion::~RuntimeTaggedUnion() = default;

auto RuntimeTaggedUnion::Tag() const -> std::size_t {
  return tag_index_;
}

auto RuntimeTaggedUnion::Member(std::size_t index) const -> RuntimeValue {
  if (index != tag_index_) {
    throw SimulationError(
        "read of a tagged union member inconsistent with the current tag "
        "(LRM 11.9)");
  }
  return payload_.front();
}

void RuntimeTaggedUnion::SetMember(std::size_t index, RuntimeValue value) {
  if (index != tag_index_) {
    throw SimulationError(
        "write to a tagged union member inconsistent with the current tag "
        "(LRM 11.9)");
  }
  payload_.front() = std::move(value);
}

auto RuntimeTaggedUnion::operator==(const RuntimeTaggedUnion& other) const
    -> PackedArray {
  if (tag_index_ != other.tag_index_) {
    return PackedArray::Bit(false);
  }
  return RuntimeValueEqual(payload_.front(), other.payload_.front());
}

auto RuntimeTaggedUnion::operator!=(const RuntimeTaggedUnion& other) const
    -> PackedArray {
  return !(*this == other);
}

auto RuntimeTaggedUnion::CaseEqual(const RuntimeTaggedUnion& other) const
    -> PackedArray {
  if (tag_index_ != other.tag_index_) {
    return PackedArray::Bit(false);
  }
  return RuntimeValueCaseEqual(payload_.front(), other.payload_.front());
}

auto RuntimeTaggedUnion::IsBitIdentical(const RuntimeTaggedUnion& other) const
    -> bool {
  return tag_index_ == other.tag_index_ &&
         RuntimeValueBitIdentical(payload_.front(), other.payload_.front());
}

auto RuntimeTaggedUnion::HasUnknown() const -> bool {
  return RuntimeValueHasUnknown(payload_.front());
}

auto RuntimeTaggedUnion::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

}  // namespace lyra::value
