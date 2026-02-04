#pragma once

#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>

namespace llvm {
class Module;
}

namespace lyra::driver {

// Statistics for a single function.
struct LlvmFunctionStats {
  std::string name;
  uint64_t instructions = 0;
  uint64_t basic_blocks = 0;
};

// Collected LLVM module statistics.
struct LlvmStats {
  uint64_t global_count = 0;
  uint64_t defined_functions = 0;
  uint64_t total_bbs = 0;
  uint64_t total_insts = 0;
  std::vector<LlvmFunctionStats> func_stats;  // Sorted by instruction count
};

// Collect LLVM module statistics. Call before module is consumed by JIT.
auto CollectLlvmStats(const llvm::Module& module) -> LlvmStats;

// Print collected LLVM stats. top_n: top functions to show (0 = summary only)
void PrintLlvmStats(const LlvmStats& stats, int top_n, FILE* sink = stderr);

// Legacy: collect and print in one call.
void PrintLlvmStats(const llvm::Module& module, int top_n, FILE* sink = stderr);

}  // namespace lyra::driver
