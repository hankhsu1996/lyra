#pragma once

#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// Hands every call of something that reports a failure of the design at its
// caller where the call was written, as its last argument: a runtime entry its
// declaration does not excuse, a body of the tool's own text, and a record's or
// a type's operation answering such an entry. It is the unit's constant for the
// call's own location, or, inside a body that reports at its caller, the one
// that body was handed, as rustc hands a `track_caller` callee the location of
// the call or the one its own caller handed it. It is stated here, once for
// every call, so that no site composing a call has to know which callees take
// one, and the verifier can hold every call to it.
auto StateCallerLocationsOnBody(
    const PassContext& context, const mir::CodeSource& at,
    const mir::CallableCode& code) -> std::optional<mir::CallableCode>;

}  // namespace lyra::mir_transform
