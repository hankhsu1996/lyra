#include "lyra/runtime/value_families.hpp"

namespace lyra::runtime {

template class ValueStorageCore<value::String>;
template class ValueStorageCore<value::Real>;
template class ValueStorageCore<value::ShortReal>;
template class ValueStorageCore<value::Chandle>;
template class ValueStorageCore<value::ObjectRef>;

template class Var<value::String>;
template class Var<value::Real>;
template class Var<value::ShortReal>;
template class Var<value::Chandle>;
template class Var<value::ObjectRef>;

template class CellRareState<value::String>;
template class CellRareState<value::Real>;
template class CellRareState<value::ShortReal>;
template class CellRareState<value::ObjectRef>;

template class Ref<value::String>;
template class Ref<value::Real>;
template class Ref<value::ShortReal>;
template class Ref<value::Chandle>;
template class Ref<value::ObjectRef>;

template class ScopedMutation<Ref<value::String>>;
template class ScopedMutation<Ref<value::Real>>;
template class ScopedMutation<Ref<value::ShortReal>>;
template class ScopedMutation<Ref<value::Chandle>>;
template class ScopedMutation<Ref<value::ObjectRef>>;

template class Takeovers<value::String>;
template class Takeovers<value::Real>;
template class Takeovers<value::ShortReal>;
template class Takeovers<value::Chandle>;
template class Takeovers<value::ObjectRef>;

template class ActivationValueCell<value::String>;
template class ActivationValueCell<value::Real>;
template class ActivationValueCell<value::ShortReal>;
template class ActivationValueCell<value::Chandle>;
template class ActivationValueCell<value::ObjectRef>;

template class SampledHistory<value::String>;
template class SampledHistory<value::Real>;
template class SampledHistory<value::ShortReal>;
template class SampledHistory<value::ObjectRef>;

template class CompletionSlot<value::String>;
template class CompletionSlot<value::Real>;
template class CompletionSlot<value::ShortReal>;
template class CompletionSlot<value::Chandle>;
template class CompletionSlot<value::ObjectRef>;
template class CompletionSlot<value::Tuple<>>;

template class Coroutine<void>;
template class Coroutine<value::String>;
template class Coroutine<value::Real>;
template class Coroutine<value::ShortReal>;
template class Coroutine<value::Chandle>;
template class Coroutine<value::ObjectRef>;
template class Coroutine<value::Tuple<>>;

}  // namespace lyra::runtime
