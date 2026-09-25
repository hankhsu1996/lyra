#pragma once

#include <array>
#include <cstddef>

#include "lyra/support/value_domain.hpp"
#include "lyra/value/runtime_value.hpp"

// A runtime value and the handle generated code reads it as. A handle carries
// no domain of its own, so the two directions are not symmetric: going out
// borrows the representation the value already holds, and coming back applies
// the domain the reader states. What states it is whatever knew the type -- an
// entry's own symbol, or the record a generated body was registered under.
namespace lyra::runtime {

// The handle `value` crosses as: the address of what it holds, borrowed rather
// than copied, so the value has to outlive the call.
[[nodiscard]] auto HandleOf(const value::RuntimeValue& value) -> const void*;

// Storage a generated body builds its answer in. An erased value can hold a
// value of any domain, so storage laid out as one can too.
struct AnswerStorage {
  alignas(value::RuntimeValue)
      std::array<std::byte, sizeof(value::RuntimeValue)> bytes;
};

// The value a generated body built in `storage`, read in `domain` and taken out
// of it, which ends what was built there.
[[nodiscard]] auto TakeValue(support::ValueDomain domain, void* storage)
    -> value::RuntimeValue;

}  // namespace lyra::runtime
