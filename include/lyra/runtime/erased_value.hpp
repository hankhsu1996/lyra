#pragma once

#include "lyra/support/value_domain.hpp"
#include "lyra/value/runtime_value.hpp"

// A runtime value and the handle generated code reads it as. A handle carries
// no domain of its own, so the two directions are not symmetric: going out
// borrows the representation the value already holds, and coming back applies
// the domain the reader states. What states it is whatever knew the type -- an
// entry's own symbol, or the record a generated body was registered under.
namespace lyra::runtime {

// The handle `value` crosses as, borrowed from the value itself rather than
// copied, so the value has to outlive the call. A chandle crosses as the
// pointer it carries; every other domain crosses as the address of what it
// holds.
[[nodiscard]] auto HandleOf(const value::RuntimeValue& value) -> const void*;

// The value `handle` names, read in `domain`.
[[nodiscard]] auto ValueOf(support::ValueDomain domain, void* handle)
    -> value::RuntimeValue;

}  // namespace lyra::runtime
