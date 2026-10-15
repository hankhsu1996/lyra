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

template class ValueStorageCore<value::RuntimeDynamicArray>;
template class ValueStorageCore<value::RuntimeQueue>;
template class ValueStorageCore<value::RuntimeAssociativeArray>;

template class Cell<WordsStorage<value::StateDomain::kTwoState>>;
template class Cell<WordsStorage<value::StateDomain::kFourState>>;
template class Cell<LaidOutStorage<value::RuntimeRecord>>;
template class Cell<ValueStorageCore<value::RuntimeDynamicArray>>;
template class Cell<LaidOutStorage<value::RuntimeFixedArray>>;
template class Cell<ValueStorageCore<value::RuntimeQueue>>;
template class Cell<ValueStorageCore<value::RuntimeAssociativeArray>>;

template class TailCell<WordsStorage<value::StateDomain::kTwoState>>;
template class TailCell<WordsStorage<value::StateDomain::kFourState>>;
template class TailCell<LaidOutStorage<value::RuntimeRecord>>;
template class TailCell<LaidOutStorage<value::RuntimeFixedArray>>;
template class Var<value::RuntimeDynamicArray>;
template class Var<value::RuntimeQueue>;
template class Var<value::RuntimeAssociativeArray>;

template class CellRareState<WordsStorage<value::StateDomain::kTwoState>>;
template class CellRareState<WordsStorage<value::StateDomain::kFourState>>;
template class CellRareState<LaidOutStorage<value::RuntimeRecord>>;
template class CellRareState<ValueStorageCore<value::RuntimeDynamicArray>>;
template class CellRareState<LaidOutStorage<value::RuntimeFixedArray>>;
template class CellRareState<ValueStorageCore<value::RuntimeQueue>>;
template class CellRareState<ValueStorageCore<value::RuntimeAssociativeArray>>;

template class VariableRef<value::WideBitVector>;
template class VariableRef<value::WideLogicVector>;
template class VariableRef<value::RuntimeDynamicArray>;
template class VariableRef<value::RuntimeQueue>;
template class VariableRef<value::RuntimeAssociativeArray>;

template class ScopedMutation<VariableRef<value::WideBitVector>>;
template class ScopedMutation<VariableRef<value::WideLogicVector>>;
template class ScopedMutation<VariableRef<value::RuntimeDynamicArray>>;
template class ScopedMutation<VariableRef<value::RuntimeQueue>>;
template class ScopedMutation<VariableRef<value::RuntimeAssociativeArray>>;

template class Takeovers<value::WideBitVector>;
template class Takeovers<value::WideLogicVector>;
template class Takeovers<value::RuntimeRecord>;
template class Takeovers<value::RuntimeDynamicArray>;
template class Takeovers<value::RuntimeFixedArray>;
template class Takeovers<value::RuntimeQueue>;
template class Takeovers<value::RuntimeAssociativeArray>;

template class ValueCell<value::RuntimeDynamicArray>;
template class ValueCell<value::RuntimeQueue>;
template class ValueCell<value::RuntimeAssociativeArray>;

template class SampledHistory<value::WideBitVector>;
template class SampledHistory<value::WideLogicVector>;
template class SampledHistory<value::RuntimeRecord>;
template class SampledHistory<value::RuntimeDynamicArray>;
template class SampledHistory<value::RuntimeFixedArray>;
template class SampledHistory<value::RuntimeQueue>;
template class SampledHistory<value::RuntimeAssociativeArray>;

template class ResolvedNet<value::WideLogicVector>;
template class ResolvedNet<value::RuntimeRecord>;
template class ResolvedNet<value::RuntimeFixedArray>;

template class Driver<value::WideLogicVector>;
template class Driver<value::RuntimeRecord>;
template class Driver<value::RuntimeFixedArray>;

template class CompletionSlot<value::RuntimeRecord>;
template class CompletionSlot<value::RuntimeDynamicArray>;
template class CompletionSlot<value::RuntimeFixedArray>;
template class CompletionSlot<value::RuntimeQueue>;
template class CompletionSlot<value::RuntimeAssociativeArray>;

template class Coroutine<value::RuntimeRecord>;
template class Coroutine<value::RuntimeDynamicArray>;
template class Coroutine<value::RuntimeFixedArray>;
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

template class Cell<ValueStorageCore<value::BitVector<8>>>;
template class Cell<ValueStorageCore<value::BitVector<16>>>;
template class Cell<ValueStorageCore<value::BitVector<32>>>;
template class Cell<ValueStorageCore<value::BitVector<64>>>;
template class Cell<ValueStorageCore<value::LogicVector<8>>>;
template class Cell<ValueStorageCore<value::LogicVector<16>>>;
template class Cell<ValueStorageCore<value::LogicVector<32>>>;
template class Cell<ValueStorageCore<value::LogicVector<64>>>;

template class Var<value::BitVector<8>>;
template class Var<value::BitVector<16>>;
template class Var<value::BitVector<32>>;
template class Var<value::BitVector<64>>;
template class Var<value::LogicVector<8>>;
template class Var<value::LogicVector<16>>;
template class Var<value::LogicVector<32>>;
template class Var<value::LogicVector<64>>;

template class CellRareState<ValueStorageCore<value::BitVector<8>>>;
template class CellRareState<ValueStorageCore<value::BitVector<16>>>;
template class CellRareState<ValueStorageCore<value::BitVector<32>>>;
template class CellRareState<ValueStorageCore<value::BitVector<64>>>;
template class CellRareState<ValueStorageCore<value::LogicVector<8>>>;
template class CellRareState<ValueStorageCore<value::LogicVector<16>>>;
template class CellRareState<ValueStorageCore<value::LogicVector<32>>>;
template class CellRareState<ValueStorageCore<value::LogicVector<64>>>;

template class VariableRef<value::BitVector<8>>;
template class VariableRef<value::BitVector<16>>;
template class VariableRef<value::BitVector<32>>;
template class VariableRef<value::BitVector<64>>;
template class VariableRef<value::LogicVector<8>>;
template class VariableRef<value::LogicVector<16>>;
template class VariableRef<value::LogicVector<32>>;
template class VariableRef<value::LogicVector<64>>;

template class ScopedMutation<VariableRef<value::BitVector<8>>>;
template class ScopedMutation<VariableRef<value::BitVector<16>>>;
template class ScopedMutation<VariableRef<value::BitVector<32>>>;
template class ScopedMutation<VariableRef<value::BitVector<64>>>;
template class ScopedMutation<VariableRef<value::LogicVector<8>>>;
template class ScopedMutation<VariableRef<value::LogicVector<16>>>;
template class ScopedMutation<VariableRef<value::LogicVector<32>>>;
template class ScopedMutation<VariableRef<value::LogicVector<64>>>;

template class Takeovers<value::BitVector<8>>;
template class Takeovers<value::BitVector<16>>;
template class Takeovers<value::BitVector<32>>;
template class Takeovers<value::BitVector<64>>;
template class Takeovers<value::LogicVector<8>>;
template class Takeovers<value::LogicVector<16>>;
template class Takeovers<value::LogicVector<32>>;
template class Takeovers<value::LogicVector<64>>;

template class ValueCell<value::BitVector<8>>;
template class ValueCell<value::BitVector<16>>;
template class ValueCell<value::BitVector<32>>;
template class ValueCell<value::BitVector<64>>;
template class ValueCell<value::LogicVector<8>>;
template class ValueCell<value::LogicVector<16>>;
template class ValueCell<value::LogicVector<32>>;
template class ValueCell<value::LogicVector<64>>;

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
