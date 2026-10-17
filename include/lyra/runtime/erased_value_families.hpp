#pragma once

#include "lyra/runtime/bound_members.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/value_cell.hpp"
#include "lyra/runtime/value_families.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/fixed_array_value.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// The storage families over the holders the library keeps a value in where it
// was compiled without the value's type: an integral value wider than a word,
// a product, a union, and each kind of array. A program whose bodies are
// generated against the library's entries holds every value of a design's own
// types through one of these, so each family is compiled once, here, and the
// library's own code refers to that copy. Code compiled with the design's types
// holds none of them, which is why nothing it includes states these.

extern template class ValueStorageCore<value::RuntimeDynamicArray>;
extern template class ValueStorageCore<value::RuntimeQueue>;
extern template class ValueStorageCore<value::RuntimeAssociativeArray>;

extern template class Cell<WordsStorage<value::StateDomain::kTwoState>>;
extern template class Cell<WordsStorage<value::StateDomain::kFourState>>;
extern template class Cell<LaidOutStorage>;
extern template class Cell<ValueStorageCore<value::RuntimeDynamicArray>>;
extern template class Cell<ValueStorageCore<value::RuntimeQueue>>;
extern template class Cell<ValueStorageCore<value::RuntimeAssociativeArray>>;

extern template class TailCell<WordsStorage<value::StateDomain::kTwoState>>;
extern template class TailCell<WordsStorage<value::StateDomain::kFourState>>;
extern template class TailCell<LaidOutStorage>;
extern template class Var<value::RuntimeDynamicArray>;
extern template class Var<value::RuntimeQueue>;
extern template class Var<value::RuntimeAssociativeArray>;

extern template class CellRareState<
    WordsStorage<value::StateDomain::kTwoState>>;
extern template class CellRareState<
    WordsStorage<value::StateDomain::kFourState>>;
extern template class CellRareState<LaidOutStorage>;
extern template class CellRareState<
    ValueStorageCore<value::RuntimeDynamicArray>>;
extern template class CellRareState<ValueStorageCore<value::RuntimeQueue>>;
extern template class CellRareState<
    ValueStorageCore<value::RuntimeAssociativeArray>>;

extern template class VariableRef<value::RuntimeDynamicArray>;
extern template class VariableRef<value::RuntimeQueue>;
extern template class VariableRef<value::RuntimeAssociativeArray>;

extern template class ScopedMutation<VariableRef<value::RuntimeDynamicArray>>;
extern template class ScopedMutation<VariableRef<value::RuntimeQueue>>;
extern template class ScopedMutation<
    VariableRef<value::RuntimeAssociativeArray>>;

extern template class Takeovers<value::WideBitVector>;
extern template class Takeovers<value::WideLogicVector>;
extern template class Takeovers<value::LaidOutValue>;
extern template class Takeovers<value::RuntimeDynamicArray>;
extern template class Takeovers<value::RuntimeQueue>;
extern template class Takeovers<value::RuntimeAssociativeArray>;

extern template class ValueCell<value::RuntimeDynamicArray>;
extern template class ValueCell<value::RuntimeQueue>;
extern template class ValueCell<value::RuntimeAssociativeArray>;

extern template class SampledHistory<value::WideBitVector>;
extern template class SampledHistory<value::WideLogicVector>;
extern template class SampledHistory<value::LaidOutValue>;
extern template class SampledHistory<value::RuntimeDynamicArray>;
extern template class SampledHistory<value::RuntimeQueue>;
extern template class SampledHistory<value::RuntimeAssociativeArray>;

extern template class ResolvedNet<value::WideLogicVector>;
extern template class ResolvedNet<value::LaidOutValue>;

extern template class Driver<value::WideLogicVector>;
extern template class Driver<value::LaidOutValue>;

extern template class CompletionSlot<value::LaidOutValue>;
extern template class CompletionSlot<value::RuntimeDynamicArray>;
extern template class CompletionSlot<value::RuntimeQueue>;
extern template class CompletionSlot<value::RuntimeAssociativeArray>;

extern template class Coroutine<value::LaidOutValue>;
extern template class Coroutine<value::RuntimeDynamicArray>;
extern template class Coroutine<value::RuntimeQueue>;
extern template class Coroutine<value::RuntimeAssociativeArray>;

// The same families over each layout an integral value no wider than a word
// has: the storage unit one plane takes, and whether an unknown plane follows
// it. What a holder does with the value it keeps -- storing it, copying it,
// comparing its bits for a change, folding one contribution into another --
// reads only those bytes, so a holder over a layout serves every integral
// type laid out that way, and each is stated over the widest unsigned type
// that is. A net holds only what can be high impedance (LRM 6.7.1), so only
// the four-state layouts have one.

extern template class ValueStorageCore<value::BitVector<8>>;
extern template class ValueStorageCore<value::BitVector<16>>;
extern template class ValueStorageCore<value::BitVector<32>>;
extern template class ValueStorageCore<value::BitVector<64>>;
extern template class ValueStorageCore<value::LogicVector<8>>;
extern template class ValueStorageCore<value::LogicVector<16>>;
extern template class ValueStorageCore<value::LogicVector<32>>;
extern template class ValueStorageCore<value::LogicVector<64>>;

extern template class Cell<ValueStorageCore<value::BitVector<8>>>;
extern template class Cell<ValueStorageCore<value::BitVector<16>>>;
extern template class Cell<ValueStorageCore<value::BitVector<32>>>;
extern template class Cell<ValueStorageCore<value::BitVector<64>>>;
extern template class Cell<ValueStorageCore<value::LogicVector<8>>>;
extern template class Cell<ValueStorageCore<value::LogicVector<16>>>;
extern template class Cell<ValueStorageCore<value::LogicVector<32>>>;
extern template class Cell<ValueStorageCore<value::LogicVector<64>>>;

extern template class Var<value::BitVector<8>>;
extern template class Var<value::BitVector<16>>;
extern template class Var<value::BitVector<32>>;
extern template class Var<value::BitVector<64>>;
extern template class Var<value::LogicVector<8>>;
extern template class Var<value::LogicVector<16>>;
extern template class Var<value::LogicVector<32>>;
extern template class Var<value::LogicVector<64>>;

extern template class CellRareState<ValueStorageCore<value::BitVector<8>>>;
extern template class CellRareState<ValueStorageCore<value::BitVector<16>>>;
extern template class CellRareState<ValueStorageCore<value::BitVector<32>>>;
extern template class CellRareState<ValueStorageCore<value::BitVector<64>>>;
extern template class CellRareState<ValueStorageCore<value::LogicVector<8>>>;
extern template class CellRareState<ValueStorageCore<value::LogicVector<16>>>;
extern template class CellRareState<ValueStorageCore<value::LogicVector<32>>>;
extern template class CellRareState<ValueStorageCore<value::LogicVector<64>>>;

extern template class VariableRef<value::BitVector<8>>;
extern template class VariableRef<value::BitVector<16>>;
extern template class VariableRef<value::BitVector<32>>;
extern template class VariableRef<value::BitVector<64>>;
extern template class VariableRef<value::LogicVector<8>>;
extern template class VariableRef<value::LogicVector<16>>;
extern template class VariableRef<value::LogicVector<32>>;
extern template class VariableRef<value::LogicVector<64>>;

extern template class ScopedMutation<VariableRef<value::BitVector<8>>>;
extern template class ScopedMutation<VariableRef<value::BitVector<16>>>;
extern template class ScopedMutation<VariableRef<value::BitVector<32>>>;
extern template class ScopedMutation<VariableRef<value::BitVector<64>>>;
extern template class ScopedMutation<VariableRef<value::LogicVector<8>>>;
extern template class ScopedMutation<VariableRef<value::LogicVector<16>>>;
extern template class ScopedMutation<VariableRef<value::LogicVector<32>>>;
extern template class ScopedMutation<VariableRef<value::LogicVector<64>>>;

extern template class Takeovers<value::BitVector<8>>;
extern template class Takeovers<value::BitVector<16>>;
extern template class Takeovers<value::BitVector<32>>;
extern template class Takeovers<value::BitVector<64>>;
extern template class Takeovers<value::LogicVector<8>>;
extern template class Takeovers<value::LogicVector<16>>;
extern template class Takeovers<value::LogicVector<32>>;
extern template class Takeovers<value::LogicVector<64>>;

extern template class ValueCell<value::BitVector<8>>;
extern template class ValueCell<value::BitVector<16>>;
extern template class ValueCell<value::BitVector<32>>;
extern template class ValueCell<value::BitVector<64>>;
extern template class ValueCell<value::LogicVector<8>>;
extern template class ValueCell<value::LogicVector<16>>;
extern template class ValueCell<value::LogicVector<32>>;
extern template class ValueCell<value::LogicVector<64>>;

extern template class SampledHistory<value::BitVector<8>>;
extern template class SampledHistory<value::BitVector<16>>;
extern template class SampledHistory<value::BitVector<32>>;
extern template class SampledHistory<value::BitVector<64>>;
extern template class SampledHistory<value::LogicVector<8>>;
extern template class SampledHistory<value::LogicVector<16>>;
extern template class SampledHistory<value::LogicVector<32>>;
extern template class SampledHistory<value::LogicVector<64>>;

extern template class ResolvedNet<value::LogicVector<8>>;
extern template class ResolvedNet<value::LogicVector<16>>;
extern template class ResolvedNet<value::LogicVector<32>>;
extern template class ResolvedNet<value::LogicVector<64>>;

extern template class Driver<value::LogicVector<8>>;
extern template class Driver<value::LogicVector<16>>;
extern template class Driver<value::LogicVector<32>>;
extern template class Driver<value::LogicVector<64>>;

}  // namespace lyra::runtime
