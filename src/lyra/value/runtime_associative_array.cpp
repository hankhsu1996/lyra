#include "lyra/value/runtime_associative_array.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wildcard_index.hpp"

namespace lyra::value {

auto WitnessedKey::Less::operator()(IndexView a, IndexView b) const -> bool {
  switch (order) {
    case AssociativeIndexOrder::kIndexValueDomain:
      return a.type->OrderBefore(a.bytes, b.bytes);
    // A wildcard index reaches the array as the bare value the program wrote,
    // so this normalizes per comparison where the array compiled with its
    // index type normalizes once per key. The clause admits only an integral
    // index.
    case AssociativeIndexOrder::kWildcardNumeric:
      return WildcardIndexBefore(
          WildcardIndexValue(*static_cast<const PackedArray*>(a.bytes)),
          WildcardIndexValue(*static_cast<const PackedArray*>(b.bytes)));
  }
  throw InternalError("RuntimeAssociativeArray: unknown index order");
}

RuntimeAssociativeArray::RuntimeAssociativeArray() = default;

RuntimeAssociativeArray::RuntimeAssociativeArray(
    AssociativeIndexOrder index_order, const ValueType& element,
    const void* element_default, const void* miss)
    : core_(
          std::in_place, WitnessedKey{.order = index_order},
          WitnessedElem(element, element_default), miss) {
}

RuntimeAssociativeArray::RuntimeAssociativeArray(
    const RuntimeAssociativeArray&) = default;
RuntimeAssociativeArray::RuntimeAssociativeArray(
    RuntimeAssociativeArray&&) noexcept = default;
auto RuntimeAssociativeArray::operator=(const RuntimeAssociativeArray&)
    -> RuntimeAssociativeArray& = default;
auto RuntimeAssociativeArray::operator=(RuntimeAssociativeArray&&) noexcept
    -> RuntimeAssociativeArray& = default;
RuntimeAssociativeArray::~RuntimeAssociativeArray() = default;

// An array whose declaration has not installed its element type has no
// entries to act on, so being asked to act on one is a lowering defect.
void RuntimeAssociativeArray::RequireInstalled() const {
  if (!core_.has_value()) {
    throw InternalError(
        "RuntimeAssociativeArray: an array is used before its declaration "
        "installs its element type -- please report this as a bug");
  }
}

auto RuntimeAssociativeArray::Installed() const -> const Core& {
  RequireInstalled();
  return *core_;
}

auto RuntimeAssociativeArray::Installed() -> Core& {
  RequireInstalled();
  return *core_;
}

auto RuntimeAssociativeArray::IndexOrder() const -> AssociativeIndexOrder {
  return Installed().KeyType().order;
}

auto RuntimeAssociativeArray::ElementType() const -> const ValueType& {
  return Installed().Element().Type();
}

auto RuntimeAssociativeArray::ElementDefault() const -> const void* {
  return Installed().Element().Default();
}

auto RuntimeAssociativeArray::Miss() const -> const void* {
  return Installed().Miss();
}

auto RuntimeAssociativeArray::Count() const -> std::size_t {
  return core_.has_value() ? core_->Count() : 0;
}

auto RuntimeAssociativeArray::Size() const -> PackedArray {
  return PackedArray::Int(static_cast<std::int32_t>(Count()));
}

auto RuntimeAssociativeArray::Exists(IndexView index) const -> PackedArray {
  return PackedArray::Int(Installed().Exists(index) ? 1 : 0);
}

auto RuntimeAssociativeArray::Element(IndexView index) const -> const void* {
  return Installed().ElementAt(index);
}

auto RuntimeAssociativeArray::ElementRef(IndexView index, Formation& formed)
    -> void* {
  return Installed().ElementRef(index, formed);
}

void RuntimeAssociativeArray::Store(IndexView index, const void* value) {
  Installed().Store(index, value);
}

void RuntimeAssociativeArray::Delete() {
  Installed().Clear();
}

void RuntimeAssociativeArray::DeleteIndex(IndexView index) {
  Installed().Erase(index);
}

auto RuntimeAssociativeArray::FirstIndex() const -> const AnyValue* {
  return core_.has_value() ? core_->FirstKey() : nullptr;
}

auto RuntimeAssociativeArray::LastIndex() const -> const AnyValue* {
  return core_.has_value() ? core_->LastKey() : nullptr;
}

auto RuntimeAssociativeArray::NextIndex(IndexView probe) const
    -> const AnyValue* {
  return core_.has_value() ? core_->KeyAfter(probe) : nullptr;
}

auto RuntimeAssociativeArray::PrevIndex(IndexView probe) const
    -> const AnyValue* {
  return core_.has_value() ? core_->KeyBefore(probe) : nullptr;
}

auto RuntimeAssociativeArray::Entries() const
    -> std::vector<std::pair<const AnyValue*, const void*>> {
  std::vector<std::pair<const AnyValue*, const void*>> entries;
  if (!core_.has_value()) {
    return entries;
  }
  entries.reserve(core_->Count());
  for (const auto& [index, element] : core_->Entries()) {
    entries.emplace_back(&index, element);
  }
  return entries;
}

// An array with no element type yet holds no entries, which is all a
// comparison with one can read.
auto RuntimeAssociativeArray::operator==(
    const RuntimeAssociativeArray& other) const -> PackedArray {
  if (!core_.has_value() || !other.core_.has_value()) {
    return PackedArray::Bit(Count() == other.Count());
  }
  return core_->Equal(*other.core_);
}

auto RuntimeAssociativeArray::operator!=(
    const RuntimeAssociativeArray& other) const -> PackedArray {
  return !(*this == other);
}

auto RuntimeAssociativeArray::CaseEqual(
    const RuntimeAssociativeArray& other) const -> PackedArray {
  if (!core_.has_value() || !other.core_.has_value()) {
    return PackedArray::Bit(Count() == other.Count());
  }
  return core_->CaseEqual(*other.core_);
}

auto RuntimeAssociativeArray::IsBitIdentical(
    const RuntimeAssociativeArray& other) const -> bool {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Count() == other.Count();
  }
  return core_->IsBitIdentical(*other.core_);
}

auto RuntimeAssociativeArray::HasUnknown() const -> bool {
  return core_.has_value() && core_->HasUnknown();
}

auto RuntimeAssociativeArray::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeAssociativeArray::BitstreamWidth() const -> PackedArray {
  return core_.has_value() ? core_->BitstreamWidth() : PackedArray::Int(0);
}

auto RuntimeAssociativeArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  return core_.has_value() ? core_->CountBits(control_bits)
                           : PackedArray::Int(0);
}

}  // namespace lyra::value
