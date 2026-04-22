#pragma once

namespace lyra::driver {

struct ValidatedCompilationInput;

// Run simulation using in-process ORC JIT.
auto RunJit(const ValidatedCompilationInput& input) -> int;

}  // namespace lyra::driver
