#include "llvm_stats.hpp"

#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

namespace lyra::driver {

auto CollectLlvmStats(const llvm::Module& module) -> LlvmStats {
  LlvmStats stats;

  // Count globals
  for (const auto& g : module.globals()) {
    (void)g;
    ++stats.global_count;
  }

  // Count functions and their stats
  for (const auto& f : module) {
    if (f.isDeclaration()) {
      continue;
    }
    ++stats.defined_functions;

    uint64_t func_bbs = 0;
    uint64_t func_insts = 0;

    for (const auto& bb : f) {
      ++func_bbs;
      func_insts += bb.size();
    }

    stats.total_bbs += func_bbs;
    stats.total_insts += func_insts;

    stats.func_stats.push_back({
        .name = f.getName().str(),
        .instructions = func_insts,
        .basic_blocks = func_bbs,
    });
  }

  // Sort by instruction count descending
  std::sort(
      stats.func_stats.begin(), stats.func_stats.end(),
      [](const auto& a, const auto& b) {
        return a.instructions > b.instructions;
      });

  return stats;
}

void PrintLlvmStats(const LlvmStats& stats, int top_n, FILE* sink) {
  // Print summary
  fmt::print(
      sink, "[lyra][stats] functions={} globals={} bbs={} insts={}\n",
      stats.defined_functions, stats.global_count, stats.total_bbs,
      stats.total_insts);

  // Print max function (if any defined functions exist)
  if (!stats.func_stats.empty() && stats.total_insts > 0) {
    const auto& max_func = stats.func_stats[0];
    double percent = 100.0 * static_cast<double>(max_func.instructions) /
                     static_cast<double>(stats.total_insts);
    fmt::print(
        sink, "[lyra][stats][max] name={} insts={} percent={:.1f}%\n",
        max_func.name, max_func.instructions, percent);
  }

  // Print top N functions
  int count = std::min(top_n, static_cast<int>(stats.func_stats.size()));
  for (int i = 0; i < count; ++i) {
    const auto& fs = stats.func_stats[i];
    fmt::print(
        sink, "[lyra][stats][top] {}) {} insts={} bbs={}\n", i + 1, fs.name,
        fs.instructions, fs.basic_blocks);
  }

  std::fflush(sink);
}

void PrintLlvmStats(const llvm::Module& module, int top_n, FILE* sink) {
  LlvmStats stats = CollectLlvmStats(module);
  PrintLlvmStats(stats, top_n, sink);
}

}  // namespace lyra::driver
