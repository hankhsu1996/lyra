#pragma once

namespace lyra::driver {

struct CompilationInput;

// Run simulation via AOT compilation: compile to native executable, then
// execute it. Plusargs are forwarded to the compiled binary via argv.
auto RunAot(const CompilationInput& input) -> int;

}  // namespace lyra::driver
