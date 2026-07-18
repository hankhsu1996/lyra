#include "lyra/value/runtime_tuple.hpp"

#include <algorithm>
#include <cstddef>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::value {

RuntimeTuple::RuntimeTuple() = default;
RuntimeTuple::RuntimeTuple(std::vector<RuntimeValue> components)
    : components_(std::move(components)) {
}
RuntimeTuple::RuntimeTuple(const RuntimeTuple&) = default;
RuntimeTuple::RuntimeTuple(RuntimeTuple&&) noexcept = default;
auto RuntimeTuple::operator=(const RuntimeTuple&) -> RuntimeTuple& = default;
auto RuntimeTuple::operator=(RuntimeTuple&&) noexcept
    -> RuntimeTuple& = default;
RuntimeTuple::~RuntimeTuple() = default;

auto RuntimeTuple::Component(std::size_t index) const -> const RuntimeValue& {
  if (index >= components_.size()) {
    throw InternalError("RuntimeTuple::Component: index out of range");
  }
  return components_[index];
}

void RuntimeTuple::SetComponent(std::size_t index, RuntimeValue value) {
  if (index >= components_.size()) {
    throw InternalError("RuntimeTuple::SetComponent: index out of range");
  }
  if (components_[index].value.index() != value.value.index()) {
    throw InternalError(
        "RuntimeTuple::SetComponent: replacement domain differs from the "
        "field's declared domain");
  }
  components_[index] = std::move(value);
}

namespace {

auto RequireSameCount(std::size_t a_count, std::size_t b_count) -> void {
  if (a_count != b_count) {
    throw InternalError(
        "RuntimeTuple: comparing products of different component counts");
  }
}

}  // namespace

auto RuntimeTuple::operator==(const RuntimeTuple& other) const -> PackedArray {
  RequireSameCount(components_.size(), other.components_.size());
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < components_.size(); ++i) {
    result = result && RuntimeValueEqual(components_[i], other.components_[i]);
  }
  return result;
}

auto RuntimeTuple::operator!=(const RuntimeTuple& other) const -> PackedArray {
  return !(*this == other);
}

auto RuntimeTuple::CaseEqual(const RuntimeTuple& other) const -> PackedArray {
  RequireSameCount(components_.size(), other.components_.size());
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < components_.size(); ++i) {
    result =
        result && RuntimeValueCaseEqual(components_[i], other.components_[i]);
  }
  return result;
}

auto RuntimeTuple::IsBitIdentical(const RuntimeTuple& other) const -> bool {
  if (components_.size() != other.components_.size()) {
    return false;
  }
  for (std::size_t i = 0; i < components_.size(); ++i) {
    if (!RuntimeValueBitIdentical(components_[i], other.components_[i])) {
      return false;
    }
  }
  return true;
}

auto RuntimeTuple::HasUnknown() const -> bool {
  return std::ranges::any_of(components_, [](const RuntimeValue& component) {
    return RuntimeValueHasUnknown(component);
  });
}

auto RuntimeTuple::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

}  // namespace lyra::value
