#pragma once

#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// What a declaration's storage may hold is a closed set, written out where a
// member's storage is realized. A family written over that set is therefore a
// family the library can state in full, and each declaration below says the
// library has already compiled one of them: a unit that uses it refers to that
// copy instead of compiling its own, which every unit would otherwise do for
// every family member it touches, leaving the linker to discard all but one.
//
// What a declaration here withholds is the duplicate and not the definition:
// each template is still written in the header a unit reads, so a build asking
// to be optimized reads it and inlines as it sees fit. What it needs in return
// is the matching definition in this header's own source file -- a family
// member used but not defined there is a link error rather than a silent copy
// per unit.

extern template class ValueStorageCore<value::PackedArray>;
extern template class ValueStorageCore<value::String>;
extern template class ValueStorageCore<value::Real>;
extern template class ValueStorageCore<value::ShortReal>;
extern template class ValueStorageCore<value::Chandle>;
extern template class ValueStorageCore<value::RuntimeTuple>;
extern template class ValueStorageCore<value::RuntimeUnion>;
extern template class ValueStorageCore<value::RuntimeTaggedUnion>;
extern template class ValueStorageCore<value::RuntimeDynamicArray>;
extern template class ValueStorageCore<value::RuntimeUnpackedArray>;
extern template class ValueStorageCore<value::RuntimeQueue>;
extern template class ValueStorageCore<value::RuntimeAssociativeArray>;
extern template class ValueStorageCore<value::ManagedRef>;

extern template class Var<value::PackedArray>;
extern template class Var<value::String>;
extern template class Var<value::Real>;
extern template class Var<value::ShortReal>;
extern template class Var<value::RuntimeTuple>;
extern template class Var<value::RuntimeUnion>;
extern template class Var<value::RuntimeTaggedUnion>;
extern template class Var<value::RuntimeDynamicArray>;
extern template class Var<value::RuntimeUnpackedArray>;
extern template class Var<value::RuntimeQueue>;
extern template class Var<value::RuntimeAssociativeArray>;
extern template class Var<value::ManagedRef>;

extern template class Ref<value::PackedArray>;
extern template class Ref<value::String>;
extern template class Ref<value::Real>;
extern template class Ref<value::ShortReal>;
extern template class Ref<value::RuntimeTuple>;
extern template class Ref<value::RuntimeUnion>;
extern template class Ref<value::RuntimeTaggedUnion>;
extern template class Ref<value::RuntimeDynamicArray>;
extern template class Ref<value::RuntimeUnpackedArray>;
extern template class Ref<value::RuntimeQueue>;
extern template class Ref<value::RuntimeAssociativeArray>;
extern template class Ref<value::ManagedRef>;

extern template class ScopedMutation<Ref<value::PackedArray>>;
extern template class ScopedMutation<Ref<value::String>>;
extern template class ScopedMutation<Ref<value::Real>>;
extern template class ScopedMutation<Ref<value::ShortReal>>;
extern template class ScopedMutation<Ref<value::RuntimeTuple>>;
extern template class ScopedMutation<Ref<value::RuntimeUnion>>;
extern template class ScopedMutation<Ref<value::RuntimeTaggedUnion>>;
extern template class ScopedMutation<Ref<value::RuntimeDynamicArray>>;
extern template class ScopedMutation<Ref<value::RuntimeUnpackedArray>>;
extern template class ScopedMutation<Ref<value::RuntimeQueue>>;
extern template class ScopedMutation<Ref<value::RuntimeAssociativeArray>>;
extern template class ScopedMutation<Ref<value::ManagedRef>>;

extern template class Takeovers<value::PackedArray>;
extern template class Takeovers<value::String>;
extern template class Takeovers<value::Real>;
extern template class Takeovers<value::ShortReal>;
extern template class Takeovers<value::RuntimeTuple>;
extern template class Takeovers<value::RuntimeUnion>;
extern template class Takeovers<value::RuntimeTaggedUnion>;
extern template class Takeovers<value::RuntimeDynamicArray>;
extern template class Takeovers<value::RuntimeUnpackedArray>;
extern template class Takeovers<value::RuntimeQueue>;
extern template class Takeovers<value::RuntimeAssociativeArray>;
extern template class Takeovers<value::ManagedRef>;

extern template class ActivationValueCell<value::PackedArray>;
extern template class ActivationValueCell<value::String>;
extern template class ActivationValueCell<value::Real>;
extern template class ActivationValueCell<value::ShortReal>;
extern template class ActivationValueCell<value::Chandle>;
extern template class ActivationValueCell<value::RuntimeTuple>;
extern template class ActivationValueCell<value::RuntimeUnion>;
extern template class ActivationValueCell<value::RuntimeTaggedUnion>;
extern template class ActivationValueCell<value::RuntimeDynamicArray>;
extern template class ActivationValueCell<value::RuntimeUnpackedArray>;
extern template class ActivationValueCell<value::RuntimeQueue>;
extern template class ActivationValueCell<value::RuntimeAssociativeArray>;
extern template class ActivationValueCell<value::ManagedRef>;

extern template class SampledHistory<value::PackedArray>;
extern template class SampledHistory<value::String>;
extern template class SampledHistory<value::Real>;
extern template class SampledHistory<value::ShortReal>;
extern template class SampledHistory<value::RuntimeTuple>;
extern template class SampledHistory<value::RuntimeUnion>;
extern template class SampledHistory<value::RuntimeTaggedUnion>;
extern template class SampledHistory<value::RuntimeDynamicArray>;
extern template class SampledHistory<value::RuntimeUnpackedArray>;
extern template class SampledHistory<value::RuntimeQueue>;
extern template class SampledHistory<value::RuntimeAssociativeArray>;
extern template class SampledHistory<value::ManagedRef>;

extern template class ResolvedNet<value::PackedArray>;
extern template class ResolvedNet<value::RuntimeTuple>;
extern template class ResolvedNet<value::RuntimeUnion>;
extern template class ResolvedNet<value::RuntimeUnpackedArray>;

// A body completes with nothing or with a value of one of the same domains, so
// the frame's own bookkeeping is on this list too. What stays the unit's is the
// frame the body compiles to, which is the design's own code.
extern template class Coroutine<void>;
extern template class Coroutine<value::PackedArray>;
extern template class Coroutine<value::String>;
extern template class Coroutine<value::Real>;
extern template class Coroutine<value::ShortReal>;
extern template class Coroutine<value::Chandle>;
extern template class Coroutine<value::RuntimeTuple>;
extern template class Coroutine<value::RuntimeUnion>;
extern template class Coroutine<value::RuntimeTaggedUnion>;
extern template class Coroutine<value::RuntimeDynamicArray>;
extern template class Coroutine<value::RuntimeUnpackedArray>;
extern template class Coroutine<value::RuntimeQueue>;
extern template class Coroutine<value::RuntimeAssociativeArray>;
extern template class Coroutine<value::ManagedRef>;

}  // namespace lyra::runtime
