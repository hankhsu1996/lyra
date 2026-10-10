#pragma once

#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/bound_members.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::runtime {

// The storage families over the value types no design shapes: a string, the
// two reals, a chandle, a class handle, and the empty product a body completes
// with when it produces nothing. Every design holds values of these same
// types, so a family over one is a family the library can state in full, and
// each declaration below says the library has already compiled it: a unit that
// uses it refers to that copy instead of compiling its own, which every unit
// would otherwise do for every family member it touches, leaving the linker to
// discard all but one. A value type composed from a design's own declarations
// -- an integral type, an array, a structure -- is not here: a unit compiled
// with the design's types compiles the families over them itself.
//
// A declaration here does not reach a constructor or destructor defaulted where
// it is declared, which a unit defines itself wherever it constructs one; that
// is why the families a unit holds default theirs outside the class. What each
// declaration needs in return is the matching definition in this header's own
// source file -- a family member used but not defined there is a link error
// rather than a silent copy per unit.

extern template class ValueStorageCore<value::String>;
extern template class ValueStorageCore<value::Real>;
extern template class ValueStorageCore<value::ShortReal>;
extern template class ValueStorageCore<value::Chandle>;
extern template class ValueStorageCore<value::ObjectRef>;

extern template class Var<value::String>;
extern template class Var<value::Real>;
extern template class Var<value::ShortReal>;
extern template class Var<value::Chandle>;
extern template class Var<value::ObjectRef>;

extern template class CellRareState<value::String>;
extern template class CellRareState<value::Real>;
extern template class CellRareState<value::ShortReal>;
extern template class CellRareState<value::ObjectRef>;

extern template class Ref<value::String>;
extern template class Ref<value::Real>;
extern template class Ref<value::ShortReal>;
extern template class Ref<value::Chandle>;
extern template class Ref<value::ObjectRef>;

extern template class ScopedMutation<Ref<value::String>>;
extern template class ScopedMutation<Ref<value::Real>>;
extern template class ScopedMutation<Ref<value::ShortReal>>;
extern template class ScopedMutation<Ref<value::Chandle>>;
extern template class ScopedMutation<Ref<value::ObjectRef>>;

extern template class Takeovers<value::String>;
extern template class Takeovers<value::Real>;
extern template class Takeovers<value::ShortReal>;
extern template class Takeovers<value::Chandle>;
extern template class Takeovers<value::ObjectRef>;

extern template class ActivationValueCell<value::String>;
extern template class ActivationValueCell<value::Real>;
extern template class ActivationValueCell<value::ShortReal>;
extern template class ActivationValueCell<value::Chandle>;
extern template class ActivationValueCell<value::ObjectRef>;

extern template class SampledHistory<value::String>;
extern template class SampledHistory<value::Real>;
extern template class SampledHistory<value::ShortReal>;
extern template class SampledHistory<value::ObjectRef>;

// A body completes with nothing or with a value, so the frame's own bookkeeping
// is on this list too, and the slot its outcome settles in. What stays the
// unit's is the frame the body compiles to, which is the design's own code.
extern template class CompletionSlot<value::String>;
extern template class CompletionSlot<value::Real>;
extern template class CompletionSlot<value::ShortReal>;
extern template class CompletionSlot<value::Chandle>;
extern template class CompletionSlot<value::ObjectRef>;
extern template class CompletionSlot<value::Tuple<>>;

extern template class Coroutine<void>;
extern template class Coroutine<value::String>;
extern template class Coroutine<value::Real>;
extern template class Coroutine<value::ShortReal>;
extern template class Coroutine<value::Chandle>;
extern template class Coroutine<value::ObjectRef>;
extern template class Coroutine<value::Tuple<>>;

}  // namespace lyra::runtime
