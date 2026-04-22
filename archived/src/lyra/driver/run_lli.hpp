#pragma once

namespace lyra::driver {

struct ValidatedCompilationInput;

// Run simulation using out-of-process lli (LLVM interpreter).
auto RunLli(const ValidatedCompilationInput& input) -> int;

}  // namespace lyra::driver
