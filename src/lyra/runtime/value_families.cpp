#include "lyra/runtime/value_families.hpp"

namespace lyra::runtime {

template class ValueStorageCore<value::String>;
template class ValueStorageCore<value::Real>;
template class ValueStorageCore<value::ShortReal>;
template class ValueStorageCore<value::UnmanagedRef>;
template class ValueStorageCore<value::ObjectRef>;

template class Cell<ValueStorageCore<value::String>>;
template class Cell<ValueStorageCore<value::Real>>;
template class Cell<ValueStorageCore<value::ShortReal>>;
template class Cell<ValueStorageCore<value::UnmanagedRef>>;
template class Cell<ValueStorageCore<value::ObjectRef>>;

template class Var<value::String>;
template class Var<value::Real>;
template class Var<value::ShortReal>;
template class Var<value::UnmanagedRef>;
template class Var<value::ObjectRef>;

template class CellRareState<ValueStorageCore<value::String>>;
template class CellRareState<ValueStorageCore<value::Real>>;
template class CellRareState<ValueStorageCore<value::ShortReal>>;
template class CellRareState<ValueStorageCore<value::ObjectRef>>;

template class VariableRef<value::String>;
template class VariableRef<value::Real>;
template class VariableRef<value::ShortReal>;
template class VariableRef<value::UnmanagedRef>;
template class VariableRef<value::ObjectRef>;

template class ScopedMutation<VariableRef<value::String>>;
template class ScopedMutation<VariableRef<value::Real>>;
template class ScopedMutation<VariableRef<value::ShortReal>>;
template class ScopedMutation<VariableRef<value::UnmanagedRef>>;
template class ScopedMutation<VariableRef<value::ObjectRef>>;

template class Takeovers<value::String>;
template class Takeovers<value::Real>;
template class Takeovers<value::ShortReal>;
template class Takeovers<value::UnmanagedRef>;
template class Takeovers<value::ObjectRef>;

template class ValueCell<value::String>;
template class ValueCell<value::Real>;
template class ValueCell<value::ShortReal>;
template class ValueCell<value::UnmanagedRef>;
template class ValueCell<value::ObjectRef>;

template class SampledHistory<value::String>;
template class SampledHistory<value::Real>;
template class SampledHistory<value::ShortReal>;
template class SampledHistory<value::ObjectRef>;

template class CompletionSlot<value::String>;
template class CompletionSlot<value::Real>;
template class CompletionSlot<value::ShortReal>;
template class CompletionSlot<value::UnmanagedRef>;
template class CompletionSlot<value::ObjectRef>;
template class CompletionSlot<value::Tuple<>>;

template class Coroutine<void>;
template class Coroutine<value::String>;
template class Coroutine<value::Real>;
template class Coroutine<value::ShortReal>;
template class Coroutine<value::UnmanagedRef>;
template class Coroutine<value::ObjectRef>;
template class Coroutine<value::Tuple<>>;

template class KeptPart<value::String>;
template class KeptPart<value::Real>;
template class KeptPart<value::ShortReal>;
template class KeptPart<value::UnmanagedRef>;
template class KeptPart<value::ObjectRef>;

template auto StepTo<value::String>(const value::String& key) -> PartPath;
template auto StepTo<value::ObjectRef>(const value::ObjectRef& key) -> PartPath;

}  // namespace lyra::runtime
