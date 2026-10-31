#include "lyra/value/runtime_associative_array.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wildcard_index.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

namespace {

// An index of an integral type, read as words.
auto IntegralIndex(IndexView index) -> LoadedWords {
  const IntegralValueType* type = index.type->AsIntegral();
  if (type == nullptr) {
    throw InternalError(
        "RuntimeAssociativeArray: an index ordered as an integral is no "
        "integral value -- please report this as a bug");
  }
  return type->Load(index.bytes);
}

// LRM 7.8.4: integral indices order by their numerical value at the type's
// signedness.
auto IntegralBefore(IndexView a, IndexView b) -> bool {
  return Less(IntegralIndex(a).View(), IntegralIndex(b).View()) ==
         FourStateBit::kOne;
}

auto IntegralNamesNoEntry(IndexView index) -> bool {
  return HasUnknown(IntegralIndex(index).View());
}

// A wildcard index reaches the array as the bare value the program wrote, so
// this normalizes per comparison where the array compiled with its index type
// normalizes once per key.
auto WildcardBefore(IndexView a, IndexView b) -> bool {
  return WildcardIndexBefore(
      WildcardIndexWords(IntegralIndex(a).View()),
      WildcardIndexWords(IntegralIndex(b).View()));
}

// An index of a kind the library holds as an object of its own, ordered as
// the array compiled with that index type orders it. Such an index holds no x
// or z, so each names an entry.
template <typename K>
constexpr IndexOrder kObjectIndexOrder{
    .before = [](IndexView a, IndexView b) -> bool {
      return typename AssocKeyTraits<K>::Less{}(
          *static_cast<const K*>(a.bytes), *static_cast<const K*>(b.bytes));
    },
    .names_no_entry = [](IndexView) -> bool { return false; }};

}  // namespace

extern "C" {
constinit const IndexOrder lyra_rt_integral_index_order{
    .before = &IntegralBefore, .names_no_entry = &IntegralNamesNoEntry};
constinit const IndexOrder lyra_rt_string_index_order =
    kObjectIndexOrder<String>;
constinit const IndexOrder lyra_rt_chandle_index_order =
    kObjectIndexOrder<Chandle>;
constinit const IndexOrder lyra_rt_managedref_index_order =
    kObjectIndexOrder<ObjectRef>;
constinit const IndexOrder lyra_rt_wildcard_index_index_order{
    .before = &WildcardBefore, .names_no_entry = &IntegralNamesNoEntry};
}

RuntimeAssociativeArray::RuntimeAssociativeArray() = default;

RuntimeAssociativeArray::RuntimeAssociativeArray(
    const value::IndexOrder& index_order, const value::ElementType& element)
    : core_(
          std::in_place, WitnessedKey{.order = &index_order},
          WitnessedElem(element)) {
}

RuntimeAssociativeArray::RuntimeAssociativeArray(Core core)
    : core_(std::move(core)) {
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

auto RuntimeAssociativeArray::ElementType() const -> const value::ElementType& {
  return Installed().Element().ElementType();
}

auto RuntimeAssociativeArray::WithDefault(const void* stated_default) const
    -> RuntimeAssociativeArray {
  return RuntimeAssociativeArray(Installed().WithDefault(stated_default));
}

auto RuntimeAssociativeArray::HasStatedDefault() const -> bool {
  return Installed().HasStatedDefault();
}

void RuntimeAssociativeArray::AbsentValue(void* out) const {
  Installed().AbsentValue(out);
}

auto RuntimeAssociativeArray::Count() const -> std::size_t {
  return core_.has_value() ? core_->Count() : 0;
}

auto RuntimeAssociativeArray::Size() const -> Int {
  return Int::FromInt(static_cast<std::int64_t>(Count()));
}

auto RuntimeAssociativeArray::Exists(IndexView index) const -> Int {
  return Int::FromInt(Installed().Exists(index) ? 1 : 0);
}

auto RuntimeAssociativeArray::Element(IndexView index) const -> const void* {
  return Installed().ElementAt(index);
}

auto RuntimeAssociativeArray::ElementMut(IndexView index, Formation& formed)
    -> void* {
  return Installed().ElementMut(index, formed);
}

auto RuntimeAssociativeArray::Lease(void* element) const -> ContainerLease {
  return Installed().Lease(element);
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
auto RuntimeAssociativeArray::Equal(
    const RuntimeAssociativeArray& other, const ValueOperations& element) const
    -> FourStateBit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return detail::ScalarOf(Count() == other.Count());
  }
  return core_->Equal(*other.core_, WitnessedOperations(element));
}

auto RuntimeAssociativeArray::CaseEqual(
    const RuntimeAssociativeArray& other, const ValueOperations& element) const
    -> Bit {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Bit::FromBool(Count() == other.Count());
  }
  return core_->CaseEqual(*other.core_, WitnessedOperations(element));
}

auto RuntimeAssociativeArray::IsBitIdentical(
    const RuntimeAssociativeArray& other) const -> bool {
  if (!core_.has_value() || !other.core_.has_value()) {
    return Count() == other.Count();
  }
  return core_->IsBitIdentical(*other.core_);
}

auto RuntimeAssociativeArray::HasUnknown(const ValueOperations& element) const
    -> bool {
  return core_.has_value() && core_->HasUnknown(WitnessedOperations(element));
}

auto RuntimeAssociativeArray::BitstreamWidth(
    const ValueOperations& element) const -> Int {
  return Int::FromInt(
      core_.has_value() ? core_->BitstreamWidth(WitnessedOperations(element))
                        : 0);
}

auto RuntimeAssociativeArray::CountBits(
    const ConstIntegralView& control_bits, const ValueOperations& element) const
    -> Int {
  return Int::FromInt(
      core_.has_value()
          ? core_->CountBits(control_bits, WitnessedOperations(element))
          : 0);
}

auto RuntimeAssociativeArray::WriteToStream(
    const IntegralView& stream, std::uint64_t filled,
    const ValueOperations& element) const -> std::uint64_t {
  return core_.has_value() ? core_->WriteToStream(
                                 stream, filled, WitnessedOperations(element))
                           : filled;
}

auto RuntimeAssociativeArray::ToBitVec(const ValueOperations& element) const
    -> BitVec {
  return StreamOf(*this, element);
}

}  // namespace lyra::value
