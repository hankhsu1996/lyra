#include "lyra/value/runtime_union.hpp"

#include <cstddef>
#include <utility>
#include <vector>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::value {

RuntimeUnion::RuntimeUnion() : active_(1) {
}
RuntimeUnion::RuntimeUnion(std::size_t active_index, RuntimeValue active)
    : active_index_(active_index) {
  active_.push_back(std::move(active));
}
RuntimeUnion::RuntimeUnion(const RuntimeUnion&) = default;
RuntimeUnion::RuntimeUnion(RuntimeUnion&&) noexcept = default;
auto RuntimeUnion::operator=(const RuntimeUnion&) -> RuntimeUnion& = default;
auto RuntimeUnion::operator=(RuntimeUnion&&) noexcept
    -> RuntimeUnion& = default;
RuntimeUnion::~RuntimeUnion() = default;

auto RuntimeUnion::Member(std::size_t index) const -> RuntimeValue {
  if (index != active_index_) {
    throw SimulationError(
        "reading an unpacked-union member other than the one last written is "
        "undefined (LRM 7.3) and not yet supported on this backend; please "
        "open an issue asking for support");
  }
  return active_.front();
}

void RuntimeUnion::SetActive(std::size_t index, RuntimeValue value) {
  active_index_ = index;
  active_.front() = std::move(value);
}

auto RuntimeUnion::operator==(const RuntimeUnion& other) const -> PackedArray {
  if (active_index_ != other.active_index_) {
    return PackedArray::Bit(false);
  }
  return RuntimeValueEqual(active_.front(), other.active_.front());
}

auto RuntimeUnion::operator!=(const RuntimeUnion& other) const -> PackedArray {
  return !(*this == other);
}

auto RuntimeUnion::CaseEqual(const RuntimeUnion& other) const -> PackedArray {
  if (active_index_ != other.active_index_) {
    return PackedArray::Bit(false);
  }
  return RuntimeValueCaseEqual(active_.front(), other.active_.front());
}

auto RuntimeUnion::ResolveTriState(const RuntimeUnion& other) const
    -> RuntimeUnion {
  if (active_index_ != other.active_index_) {
    if (IsBitIdentical(HighImpedanceLike(*this))) {
      return other;
    }
    if (other.IsBitIdentical(HighImpedanceLike(other))) {
      return *this;
    }
    throw SimulationError(
        "two drivers of an unpacked-union net are driving different members; "
        "SystemVerilog gives an unpacked union no defined storage overlay, so "
        "their resolution has no defined value");
  }
  return RuntimeUnion{
      active_index_,
      RuntimeValueResolveTriState(active_.front(), other.active_.front())};
}

auto RuntimeUnion::HighImpedanceLike(const RuntimeUnion& prototype)
    -> RuntimeUnion {
  return RuntimeUnion{
      prototype.active_index_,
      RuntimeValueHighImpedanceLike(prototype.active_.front())};
}

auto RuntimeUnion::IsBitIdentical(const RuntimeUnion& other) const -> bool {
  return active_index_ == other.active_index_ &&
         RuntimeValueBitIdentical(active_.front(), other.active_.front());
}

auto RuntimeUnion::HasUnknown() const -> bool {
  return RuntimeValueHasUnknown(active_.front());
}

auto RuntimeUnion::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

}  // namespace lyra::value
