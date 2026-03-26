#pragma once

namespace lyra::driver {

struct ValidatedCompilationInput;

// Run simulation via AOT compilation: compile to native executable, then
// execute it. Plusargs are forwarded to the compiled binary via argv.
auto RunAot(const ValidatedCompilationInput& input) -> int;

}  // namespace lyra::driver
