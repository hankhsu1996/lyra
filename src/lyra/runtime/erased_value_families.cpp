#include "lyra/runtime/erased_value_families.hpp"

#include <utility>

#include "lyra/support/value_domain.hpp"

namespace lyra::runtime {

namespace {

// A value domain the runtime gains is a holder type each family may need a
// compiled copy over, and nothing reports a family left without one: it
// compiles, and every unit that uses it compiles its own copy silently. This
// switch names every domain, so a domain added or removed stops the build here
// until the families have been looked at.
constexpr auto FamiliesCover(support::ValueDomain domain) -> bool {
  switch (domain) {
    case support::ValueDomain::kBit8:
    case support::ValueDomain::kBit16:
    case support::ValueDomain::kBit32:
    case support::ValueDomain::kBit64:
    case support::ValueDomain::kLogic8:
    case support::ValueDomain::kLogic16:
    case support::ValueDomain::kLogic32:
    case support::ValueDomain::kLogic64:
    case support::ValueDomain::kBitWide:
    case support::ValueDomain::kLogicWide:
    case support::ValueDomain::kWildcardIndex:
    case support::ValueDomain::kString:
    case support::ValueDomain::kReal:
    case support::ValueDomain::kShortReal:
    case support::ValueDomain::kChandle:
    case support::ValueDomain::kEmpty:
    case support::ValueDomain::kTuple:
    case support::ValueDomain::kUnion:
    case support::ValueDomain::kTaggedUnion:
    case support::ValueDomain::kDynArray:
    case support::ValueDomain::kUnpackedArray:
    case support::ValueDomain::kQueue:
    case support::ValueDomain::kAssocArray:
    case support::ValueDomain::kManagedRef:
      return true;
  }
  std::unreachable();
}
static_assert(FamiliesCover(support::ValueDomain::kLogicWide));

}  // namespace

template class ValueStorageCore<value::WideBitVector>;
template class ValueStorageCore<value::WideLogicVector>;
template class ValueStorageCore<value::RuntimeTuple>;
template class ValueStorageCore<value::RuntimeUnion>;
template class ValueStorageCore<value::RuntimeTaggedUnion>;
template class ValueStorageCore<value::RuntimeDynamicArray>;
template class ValueStorageCore<value::RuntimeUnpackedArray>;
template class ValueStorageCore<value::RuntimeQueue>;
template class ValueStorageCore<value::RuntimeAssociativeArray>;

template class Var<value::WideBitVector>;
template class Var<value::WideLogicVector>;
template class Var<value::RuntimeTuple>;
template class Var<value::RuntimeUnion>;
template class Var<value::RuntimeTaggedUnion>;
template class Var<value::RuntimeDynamicArray>;
template class Var<value::RuntimeUnpackedArray>;
template class Var<value::RuntimeQueue>;
template class Var<value::RuntimeAssociativeArray>;

template class CellRareState<value::WideBitVector>;
template class CellRareState<value::WideLogicVector>;
template class CellRareState<value::RuntimeTuple>;
template class CellRareState<value::RuntimeUnion>;
template class CellRareState<value::RuntimeTaggedUnion>;
template class CellRareState<value::RuntimeDynamicArray>;
template class CellRareState<value::RuntimeUnpackedArray>;
template class CellRareState<value::RuntimeQueue>;
template class CellRareState<value::RuntimeAssociativeArray>;

template class Ref<value::WideBitVector>;
template class Ref<value::WideLogicVector>;
template class Ref<value::RuntimeTuple>;
template class Ref<value::RuntimeUnion>;
template class Ref<value::RuntimeTaggedUnion>;
template class Ref<value::RuntimeDynamicArray>;
template class Ref<value::RuntimeUnpackedArray>;
template class Ref<value::RuntimeQueue>;
template class Ref<value::RuntimeAssociativeArray>;

template class ScopedMutation<Ref<value::WideBitVector>>;
template class ScopedMutation<Ref<value::WideLogicVector>>;
template class ScopedMutation<Ref<value::RuntimeTuple>>;
template class ScopedMutation<Ref<value::RuntimeUnion>>;
template class ScopedMutation<Ref<value::RuntimeTaggedUnion>>;
template class ScopedMutation<Ref<value::RuntimeDynamicArray>>;
template class ScopedMutation<Ref<value::RuntimeUnpackedArray>>;
template class ScopedMutation<Ref<value::RuntimeQueue>>;
template class ScopedMutation<Ref<value::RuntimeAssociativeArray>>;

template class Takeovers<value::WideBitVector>;
template class Takeovers<value::WideLogicVector>;
template class Takeovers<value::RuntimeTuple>;
template class Takeovers<value::RuntimeUnion>;
template class Takeovers<value::RuntimeTaggedUnion>;
template class Takeovers<value::RuntimeDynamicArray>;
template class Takeovers<value::RuntimeUnpackedArray>;
template class Takeovers<value::RuntimeQueue>;
template class Takeovers<value::RuntimeAssociativeArray>;

template class ActivationValueCell<value::WideBitVector>;
template class ActivationValueCell<value::WideLogicVector>;
template class ActivationValueCell<value::RuntimeTuple>;
template class ActivationValueCell<value::RuntimeUnion>;
template class ActivationValueCell<value::RuntimeTaggedUnion>;
template class ActivationValueCell<value::RuntimeDynamicArray>;
template class ActivationValueCell<value::RuntimeUnpackedArray>;
template class ActivationValueCell<value::RuntimeQueue>;
template class ActivationValueCell<value::RuntimeAssociativeArray>;

template class SampledHistory<value::WideBitVector>;
template class SampledHistory<value::WideLogicVector>;
template class SampledHistory<value::RuntimeTuple>;
template class SampledHistory<value::RuntimeUnion>;
template class SampledHistory<value::RuntimeTaggedUnion>;
template class SampledHistory<value::RuntimeDynamicArray>;
template class SampledHistory<value::RuntimeUnpackedArray>;
template class SampledHistory<value::RuntimeQueue>;
template class SampledHistory<value::RuntimeAssociativeArray>;

template class ResolvedNet<value::WideLogicVector>;
template class ResolvedNet<value::RuntimeTuple>;
template class ResolvedNet<value::RuntimeUnion>;
template class ResolvedNet<value::RuntimeUnpackedArray>;

template class Driver<value::WideLogicVector>;
template class Driver<value::RuntimeTuple>;
template class Driver<value::RuntimeUnion>;
template class Driver<value::RuntimeUnpackedArray>;

template class CompletionSlot<value::RuntimeTuple>;
template class CompletionSlot<value::RuntimeUnion>;
template class CompletionSlot<value::RuntimeTaggedUnion>;
template class CompletionSlot<value::RuntimeDynamicArray>;
template class CompletionSlot<value::RuntimeUnpackedArray>;
template class CompletionSlot<value::RuntimeQueue>;
template class CompletionSlot<value::RuntimeAssociativeArray>;

template class Coroutine<value::RuntimeTuple>;
template class Coroutine<value::RuntimeUnion>;
template class Coroutine<value::RuntimeTaggedUnion>;
template class Coroutine<value::RuntimeDynamicArray>;
template class Coroutine<value::RuntimeUnpackedArray>;
template class Coroutine<value::RuntimeQueue>;
template class Coroutine<value::RuntimeAssociativeArray>;

template class ValueStorageCore<value::BitVector<8>>;
template class ValueStorageCore<value::BitVector<16>>;
template class ValueStorageCore<value::BitVector<32>>;
template class ValueStorageCore<value::BitVector<64>>;
template class ValueStorageCore<value::LogicVector<8>>;
template class ValueStorageCore<value::LogicVector<16>>;
template class ValueStorageCore<value::LogicVector<32>>;
template class ValueStorageCore<value::LogicVector<64>>;

template class Var<value::BitVector<8>>;
template class Var<value::BitVector<16>>;
template class Var<value::BitVector<32>>;
template class Var<value::BitVector<64>>;
template class Var<value::LogicVector<8>>;
template class Var<value::LogicVector<16>>;
template class Var<value::LogicVector<32>>;
template class Var<value::LogicVector<64>>;

template class CellRareState<value::BitVector<8>>;
template class CellRareState<value::BitVector<16>>;
template class CellRareState<value::BitVector<32>>;
template class CellRareState<value::BitVector<64>>;
template class CellRareState<value::LogicVector<8>>;
template class CellRareState<value::LogicVector<16>>;
template class CellRareState<value::LogicVector<32>>;
template class CellRareState<value::LogicVector<64>>;

template class Ref<value::BitVector<8>>;
template class Ref<value::BitVector<16>>;
template class Ref<value::BitVector<32>>;
template class Ref<value::BitVector<64>>;
template class Ref<value::LogicVector<8>>;
template class Ref<value::LogicVector<16>>;
template class Ref<value::LogicVector<32>>;
template class Ref<value::LogicVector<64>>;

template class ScopedMutation<Ref<value::BitVector<8>>>;
template class ScopedMutation<Ref<value::BitVector<16>>>;
template class ScopedMutation<Ref<value::BitVector<32>>>;
template class ScopedMutation<Ref<value::BitVector<64>>>;
template class ScopedMutation<Ref<value::LogicVector<8>>>;
template class ScopedMutation<Ref<value::LogicVector<16>>>;
template class ScopedMutation<Ref<value::LogicVector<32>>>;
template class ScopedMutation<Ref<value::LogicVector<64>>>;

template class Takeovers<value::BitVector<8>>;
template class Takeovers<value::BitVector<16>>;
template class Takeovers<value::BitVector<32>>;
template class Takeovers<value::BitVector<64>>;
template class Takeovers<value::LogicVector<8>>;
template class Takeovers<value::LogicVector<16>>;
template class Takeovers<value::LogicVector<32>>;
template class Takeovers<value::LogicVector<64>>;

template class ActivationValueCell<value::BitVector<8>>;
template class ActivationValueCell<value::BitVector<16>>;
template class ActivationValueCell<value::BitVector<32>>;
template class ActivationValueCell<value::BitVector<64>>;
template class ActivationValueCell<value::LogicVector<8>>;
template class ActivationValueCell<value::LogicVector<16>>;
template class ActivationValueCell<value::LogicVector<32>>;
template class ActivationValueCell<value::LogicVector<64>>;

template class SampledHistory<value::BitVector<8>>;
template class SampledHistory<value::BitVector<16>>;
template class SampledHistory<value::BitVector<32>>;
template class SampledHistory<value::BitVector<64>>;
template class SampledHistory<value::LogicVector<8>>;
template class SampledHistory<value::LogicVector<16>>;
template class SampledHistory<value::LogicVector<32>>;
template class SampledHistory<value::LogicVector<64>>;

template class ResolvedNet<value::LogicVector<8>>;
template class ResolvedNet<value::LogicVector<16>>;
template class ResolvedNet<value::LogicVector<32>>;
template class ResolvedNet<value::LogicVector<64>>;

template class Driver<value::LogicVector<8>>;
template class Driver<value::LogicVector<16>>;
template class Driver<value::LogicVector<32>>;
template class Driver<value::LogicVector<64>>;

}  // namespace lyra::runtime
