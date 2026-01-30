#pragma once

namespace lyra::driver {

struct CompilationInput;

// Run simulation using out-of-process lli (LLVM interpreter).
auto RunLli(const CompilationInput& input) -> int;

}  // namespace lyra::driver
