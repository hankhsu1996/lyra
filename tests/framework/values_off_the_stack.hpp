#pragma once

#include <span>
#include <string>

namespace lyra::test {

// Builds a design whose every value is twice the stack it is run on, with
// `lyra build` given `build_options` -- which backend, and which host compiler
// where that matters -- and runs it on a stack of half a megabyte, expecting
// it to end as the design says it should. Where a value lies is the compiler's
// decision, so the same design answers for both backends.
auto ExpectValuesKeptOffTheStack(std::span<const std::string> build_options)
    -> void;

}  // namespace lyra::test
