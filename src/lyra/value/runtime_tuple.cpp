#include "lyra/value/runtime_tuple.hpp"

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"

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

auto SameDomain(const RuntimeValue& a, const RuntimeValue& b) -> void {
  if (a.value.index() != b.value.index()) {
    throw InternalError(
        "RuntimeTuple: comparing components of different runtime domains");
  }
}

auto ComponentEqual(const RuntimeValue& a, const RuntimeValue& b)
    -> PackedArray {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> PackedArray {
        using T = std::decay_t<decltype(lhs)>;
        return lhs == std::get<T>(b.value);
      },
      a.value);
}

auto ComponentCaseEqual(const RuntimeValue& a, const RuntimeValue& b)
    -> PackedArray {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> PackedArray {
        using T = std::decay_t<decltype(lhs)>;
        if constexpr (std::is_same_v<T, Real> || std::is_same_v<T, ShortReal>) {
          throw InternalError(
              "RuntimeTuple::CaseEqual: === is not defined on a real component "
              "(LRM Table 11-1)");
        } else {
          return lhs.CaseEqual(std::get<T>(b.value));
        }
      },
      a.value);
}

auto ComponentBitIdentical(const RuntimeValue& a, const RuntimeValue& b)
    -> bool {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> bool {
        using T = std::decay_t<decltype(lhs)>;
        return lhs.IsBitIdentical(std::get<T>(b.value));
      },
      a.value);
}

}  // namespace

auto RuntimeTuple::operator==(const RuntimeTuple& other) const -> PackedArray {
  RequireSameCount(components_.size(), other.components_.size());
  PackedArray result = PackedArray::Bit(true);
  for (std::size_t i = 0; i < components_.size(); ++i) {
    result = result && ComponentEqual(components_[i], other.components_[i]);
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
    result = result && ComponentCaseEqual(components_[i], other.components_[i]);
  }
  return result;
}

auto RuntimeTuple::IsBitIdentical(const RuntimeTuple& other) const -> bool {
  if (components_.size() != other.components_.size()) {
    return false;
  }
  for (std::size_t i = 0; i < components_.size(); ++i) {
    if (!ComponentBitIdentical(components_[i], other.components_[i])) {
      return false;
    }
  }
  return true;
}

auto RuntimeTuple::HasUnknown() const -> bool {
  for (const RuntimeValue& component : components_) {
    const bool unknown = std::visit(
        [](const auto& value) -> bool { return value.HasUnknown(); },
        component.value);
    if (unknown) {
      return true;
    }
  }
  return false;
}

auto RuntimeTuple::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

}  // namespace lyra::value
