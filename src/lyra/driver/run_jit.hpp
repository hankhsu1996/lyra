#pragma once

namespace lyra::driver {

struct CompilationInput;

// Run simulation using in-process ORC JIT.
auto RunJit(const CompilationInput& input) -> int;

}  // namespace lyra::driver
