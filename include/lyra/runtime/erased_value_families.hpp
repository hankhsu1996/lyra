#pragma once

#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/bound_members.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/value_families.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// The storage families over the holders the library keeps a value in where it
// was compiled without the value's type: an integral value wider than a word,
// a product, a union, and each kind of array. A program whose bodies are
// generated against the library's entries holds every value of a design's own
// types through one of these, so each family is compiled once, here, and the
// library's own code refers to that copy. Code compiled with the design's types
// holds none of them, which is why nothing it includes states these.

extern template class ValueStorageCore<value::WideBitVector>;
extern template class ValueStorageCore<value::WideLogicVector>;
extern template class ValueStorageCore<value::RuntimeTuple>;
extern template class ValueStorageCore<value::RuntimeUnion>;
extern template class ValueStorageCore<value::RuntimeTaggedUnion>;
extern template class ValueStorageCore<value::RuntimeDynamicArray>;
extern template class ValueStorageCore<value::RuntimeUnpackedArray>;
extern template class ValueStorageCore<value::RuntimeQueue>;
extern template class ValueStorageCore<value::RuntimeAssociativeArray>;

extern template class Var<value::WideBitVector>;
extern template class Var<value::WideLogicVector>;
extern template class Var<value::RuntimeTuple>;
extern template class Var<value::RuntimeUnion>;
extern template class Var<value::RuntimeTaggedUnion>;
extern template class Var<value::RuntimeDynamicArray>;
extern template class Var<value::RuntimeUnpackedArray>;
extern template class Var<value::RuntimeQueue>;
extern template class Var<value::RuntimeAssociativeArray>;

extern template class CellRareState<value::WideBitVector>;
extern template class CellRareState<value::WideLogicVector>;
extern template class CellRareState<value::RuntimeTuple>;
extern template class CellRareState<value::RuntimeUnion>;
extern template class CellRareState<value::RuntimeTaggedUnion>;
extern template class CellRareState<value::RuntimeDynamicArray>;
extern template class CellRareState<value::RuntimeUnpackedArray>;
extern template class CellRareState<value::RuntimeQueue>;
extern template class CellRareState<value::RuntimeAssociativeArray>;

extern template class Ref<value::WideBitVector>;
extern template class Ref<value::WideLogicVector>;
extern template class Ref<value::RuntimeTuple>;
extern template class Ref<value::RuntimeUnion>;
extern template class Ref<value::RuntimeTaggedUnion>;
extern template class Ref<value::RuntimeDynamicArray>;
extern template class Ref<value::RuntimeUnpackedArray>;
extern template class Ref<value::RuntimeQueue>;
extern template class Ref<value::RuntimeAssociativeArray>;

extern template class ScopedMutation<Ref<value::WideBitVector>>;
extern template class ScopedMutation<Ref<value::WideLogicVector>>;
extern template class ScopedMutation<Ref<value::RuntimeTuple>>;
extern template class ScopedMutation<Ref<value::RuntimeUnion>>;
extern template class ScopedMutation<Ref<value::RuntimeTaggedUnion>>;
extern template class ScopedMutation<Ref<value::RuntimeDynamicArray>>;
extern template class ScopedMutation<Ref<value::RuntimeUnpackedArray>>;
extern template class ScopedMutation<Ref<value::RuntimeQueue>>;
extern template class ScopedMutation<Ref<value::RuntimeAssociativeArray>>;

extern template class Takeovers<value::WideBitVector>;
extern template class Takeovers<value::WideLogicVector>;
extern template class Takeovers<value::RuntimeTuple>;
extern template class Takeovers<value::RuntimeUnion>;
extern template class Takeovers<value::RuntimeTaggedUnion>;
extern template class Takeovers<value::RuntimeDynamicArray>;
extern template class Takeovers<value::RuntimeUnpackedArray>;
extern template class Takeovers<value::RuntimeQueue>;
extern template class Takeovers<value::RuntimeAssociativeArray>;

extern template class ActivationValueCell<value::WideBitVector>;
extern template class ActivationValueCell<value::WideLogicVector>;
extern template class ActivationValueCell<value::RuntimeTuple>;
extern template class ActivationValueCell<value::RuntimeUnion>;
extern template class ActivationValueCell<value::RuntimeTaggedUnion>;
extern template class ActivationValueCell<value::RuntimeDynamicArray>;
extern template class ActivationValueCell<value::RuntimeUnpackedArray>;
extern template class ActivationValueCell<value::RuntimeQueue>;
extern template class ActivationValueCell<value::RuntimeAssociativeArray>;

extern template class SampledHistory<value::WideBitVector>;
extern template class SampledHistory<value::WideLogicVector>;
extern template class SampledHistory<value::RuntimeTuple>;
extern template class SampledHistory<value::RuntimeUnion>;
extern template class SampledHistory<value::RuntimeTaggedUnion>;
extern template class SampledHistory<value::RuntimeDynamicArray>;
extern template class SampledHistory<value::RuntimeUnpackedArray>;
extern template class SampledHistory<value::RuntimeQueue>;
extern template class SampledHistory<value::RuntimeAssociativeArray>;

extern template class ResolvedNet<value::WideLogicVector>;
extern template class ResolvedNet<value::RuntimeTuple>;
extern template class ResolvedNet<value::RuntimeUnion>;
extern template class ResolvedNet<value::RuntimeUnpackedArray>;

extern template class Driver<value::WideLogicVector>;
extern template class Driver<value::RuntimeTuple>;
extern template class Driver<value::RuntimeUnion>;
extern template class Driver<value::RuntimeUnpackedArray>;

extern template class CompletionSlot<value::RuntimeTuple>;
extern template class CompletionSlot<value::RuntimeUnion>;
extern template class CompletionSlot<value::RuntimeTaggedUnion>;
extern template class CompletionSlot<value::RuntimeDynamicArray>;
extern template class CompletionSlot<value::RuntimeUnpackedArray>;
extern template class CompletionSlot<value::RuntimeQueue>;
extern template class CompletionSlot<value::RuntimeAssociativeArray>;

extern template class Coroutine<value::RuntimeTuple>;
extern template class Coroutine<value::RuntimeUnion>;
extern template class Coroutine<value::RuntimeTaggedUnion>;
extern template class Coroutine<value::RuntimeDynamicArray>;
extern template class Coroutine<value::RuntimeUnpackedArray>;
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

extern template class Var<value::BitVector<8>>;
extern template class Var<value::BitVector<16>>;
extern template class Var<value::BitVector<32>>;
extern template class Var<value::BitVector<64>>;
extern template class Var<value::LogicVector<8>>;
extern template class Var<value::LogicVector<16>>;
extern template class Var<value::LogicVector<32>>;
extern template class Var<value::LogicVector<64>>;

extern template class CellRareState<value::BitVector<8>>;
extern template class CellRareState<value::BitVector<16>>;
extern template class CellRareState<value::BitVector<32>>;
extern template class CellRareState<value::BitVector<64>>;
extern template class CellRareState<value::LogicVector<8>>;
extern template class CellRareState<value::LogicVector<16>>;
extern template class CellRareState<value::LogicVector<32>>;
extern template class CellRareState<value::LogicVector<64>>;

extern template class Ref<value::BitVector<8>>;
extern template class Ref<value::BitVector<16>>;
extern template class Ref<value::BitVector<32>>;
extern template class Ref<value::BitVector<64>>;
extern template class Ref<value::LogicVector<8>>;
extern template class Ref<value::LogicVector<16>>;
extern template class Ref<value::LogicVector<32>>;
extern template class Ref<value::LogicVector<64>>;

extern template class ScopedMutation<Ref<value::BitVector<8>>>;
extern template class ScopedMutation<Ref<value::BitVector<16>>>;
extern template class ScopedMutation<Ref<value::BitVector<32>>>;
extern template class ScopedMutation<Ref<value::BitVector<64>>>;
extern template class ScopedMutation<Ref<value::LogicVector<8>>>;
extern template class ScopedMutation<Ref<value::LogicVector<16>>>;
extern template class ScopedMutation<Ref<value::LogicVector<32>>>;
extern template class ScopedMutation<Ref<value::LogicVector<64>>>;

extern template class Takeovers<value::BitVector<8>>;
extern template class Takeovers<value::BitVector<16>>;
extern template class Takeovers<value::BitVector<32>>;
extern template class Takeovers<value::BitVector<64>>;
extern template class Takeovers<value::LogicVector<8>>;
extern template class Takeovers<value::LogicVector<16>>;
extern template class Takeovers<value::LogicVector<32>>;
extern template class Takeovers<value::LogicVector<64>>;

extern template class ActivationValueCell<value::BitVector<8>>;
extern template class ActivationValueCell<value::BitVector<16>>;
extern template class ActivationValueCell<value::BitVector<32>>;
extern template class ActivationValueCell<value::BitVector<64>>;
extern template class ActivationValueCell<value::LogicVector<8>>;
extern template class ActivationValueCell<value::LogicVector<16>>;
extern template class ActivationValueCell<value::LogicVector<32>>;
extern template class ActivationValueCell<value::LogicVector<64>>;

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
