#pragma once

#include <cstdio>

namespace llvm {
class Module;
}

namespace lyra::driver {

// Print LLVM module statistics to the given sink (default stderr).
// top_n: number of top functions to show by instruction count (0 = summary
// only)
void PrintLlvmStats(const llvm::Module& module, int top_n, FILE* sink = stderr);

}  // namespace lyra::driver
