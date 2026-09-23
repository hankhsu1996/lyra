#include "lyra/runtime/value_families.hpp"

#include <variant>

#include "lyra/value/runtime_value.hpp"

namespace lyra::runtime {

// Every family below is stated over the domains a value may have, so a domain
// gained is a domain each of them needs. Nothing would otherwise report a
// family that missed one: it compiles, and every unit that uses it goes back to
// compiling its own copy silently. This is what fails instead.
static_assert(
    std::variant_size_v<decltype(value::RuntimeValue::value)> == 14,
    "a value domain was added or removed -- give each family below the same "
    "treatment and then correct this count");

template class ValueStorageCore<value::PackedArray>;
template class ValueStorageCore<value::String>;
template class ValueStorageCore<value::Real>;
template class ValueStorageCore<value::ShortReal>;
template class ValueStorageCore<value::Chandle>;
template class ValueStorageCore<value::RuntimeTuple>;
template class ValueStorageCore<value::RuntimeUnion>;
template class ValueStorageCore<value::RuntimeTaggedUnion>;
template class ValueStorageCore<value::RuntimeDynamicArray>;
template class ValueStorageCore<value::RuntimeUnpackedArray>;
template class ValueStorageCore<value::RuntimeQueue>;
template class ValueStorageCore<value::RuntimeAssociativeArray>;
template class ValueStorageCore<value::ManagedRef>;

template class Var<value::PackedArray>;
template class Var<value::String>;
template class Var<value::Real>;
template class Var<value::ShortReal>;
template class Var<value::RuntimeTuple>;
template class Var<value::RuntimeUnion>;
template class Var<value::RuntimeTaggedUnion>;
template class Var<value::RuntimeDynamicArray>;
template class Var<value::RuntimeUnpackedArray>;
template class Var<value::RuntimeQueue>;
template class Var<value::RuntimeAssociativeArray>;
template class Var<value::ManagedRef>;

template class Ref<value::PackedArray>;
template class Ref<value::String>;
template class Ref<value::Real>;
template class Ref<value::ShortReal>;
template class Ref<value::RuntimeTuple>;
template class Ref<value::RuntimeUnion>;
template class Ref<value::RuntimeTaggedUnion>;
template class Ref<value::RuntimeDynamicArray>;
template class Ref<value::RuntimeUnpackedArray>;
template class Ref<value::RuntimeQueue>;
template class Ref<value::RuntimeAssociativeArray>;
template class Ref<value::ManagedRef>;

template class ScopedMutation<Ref<value::PackedArray>>;
template class ScopedMutation<Ref<value::String>>;
template class ScopedMutation<Ref<value::Real>>;
template class ScopedMutation<Ref<value::ShortReal>>;
template class ScopedMutation<Ref<value::RuntimeTuple>>;
template class ScopedMutation<Ref<value::RuntimeUnion>>;
template class ScopedMutation<Ref<value::RuntimeTaggedUnion>>;
template class ScopedMutation<Ref<value::RuntimeDynamicArray>>;
template class ScopedMutation<Ref<value::RuntimeUnpackedArray>>;
template class ScopedMutation<Ref<value::RuntimeQueue>>;
template class ScopedMutation<Ref<value::RuntimeAssociativeArray>>;
template class ScopedMutation<Ref<value::ManagedRef>>;

template class Takeovers<value::PackedArray>;
template class Takeovers<value::String>;
template class Takeovers<value::Real>;
template class Takeovers<value::ShortReal>;
template class Takeovers<value::RuntimeTuple>;
template class Takeovers<value::RuntimeUnion>;
template class Takeovers<value::RuntimeTaggedUnion>;
template class Takeovers<value::RuntimeDynamicArray>;
template class Takeovers<value::RuntimeUnpackedArray>;
template class Takeovers<value::RuntimeQueue>;
template class Takeovers<value::RuntimeAssociativeArray>;
template class Takeovers<value::ManagedRef>;

template class ActivationValueCell<value::PackedArray>;
template class ActivationValueCell<value::String>;
template class ActivationValueCell<value::Real>;
template class ActivationValueCell<value::ShortReal>;
template class ActivationValueCell<value::Chandle>;
template class ActivationValueCell<value::RuntimeTuple>;
template class ActivationValueCell<value::RuntimeUnion>;
template class ActivationValueCell<value::RuntimeTaggedUnion>;
template class ActivationValueCell<value::RuntimeDynamicArray>;
template class ActivationValueCell<value::RuntimeUnpackedArray>;
template class ActivationValueCell<value::RuntimeQueue>;
template class ActivationValueCell<value::RuntimeAssociativeArray>;
template class ActivationValueCell<value::ManagedRef>;

template class SampledHistory<value::PackedArray>;
template class SampledHistory<value::String>;
template class SampledHistory<value::Real>;
template class SampledHistory<value::ShortReal>;
template class SampledHistory<value::RuntimeTuple>;
template class SampledHistory<value::RuntimeUnion>;
template class SampledHistory<value::RuntimeTaggedUnion>;
template class SampledHistory<value::RuntimeDynamicArray>;
template class SampledHistory<value::RuntimeUnpackedArray>;
template class SampledHistory<value::RuntimeQueue>;
template class SampledHistory<value::RuntimeAssociativeArray>;
template class SampledHistory<value::ManagedRef>;

template class ResolvedNet<value::PackedArray>;
template class ResolvedNet<value::RuntimeTuple>;
template class ResolvedNet<value::RuntimeUnion>;
template class ResolvedNet<value::RuntimeUnpackedArray>;

template class Coroutine<void>;
template class Coroutine<value::PackedArray>;
template class Coroutine<value::String>;
template class Coroutine<value::Real>;
template class Coroutine<value::ShortReal>;
template class Coroutine<value::Chandle>;
template class Coroutine<value::RuntimeTuple>;
template class Coroutine<value::RuntimeUnion>;
template class Coroutine<value::RuntimeTaggedUnion>;
template class Coroutine<value::RuntimeDynamicArray>;
template class Coroutine<value::RuntimeUnpackedArray>;
template class Coroutine<value::RuntimeQueue>;
template class Coroutine<value::RuntimeAssociativeArray>;
template class Coroutine<value::ManagedRef>;

}  // namespace lyra::runtime
