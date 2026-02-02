#include "llvm_stats.hpp"

#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

namespace lyra::driver {

namespace {

struct FunctionStats {
  std::string name;
  uint64_t instructions;
  uint64_t basic_blocks;
};

}  // namespace

void PrintLlvmStats(const llvm::Module& module, int top_n, FILE* sink) {
  uint64_t global_count = 0;
  uint64_t defined_functions = 0;
  uint64_t total_bbs = 0;
  uint64_t total_insts = 0;

  std::vector<FunctionStats> func_stats;

  // Count globals
  for (const auto& g : module.globals()) {
    (void)g;
    ++global_count;
  }

  // Count functions and their stats
  for (const auto& f : module) {
    if (f.isDeclaration()) {
      continue;
    }
    ++defined_functions;

    uint64_t func_bbs = 0;
    uint64_t func_insts = 0;

    for (const auto& bb : f) {
      ++func_bbs;
      func_insts += bb.size();
    }

    total_bbs += func_bbs;
    total_insts += func_insts;

    func_stats.push_back({
        .name = f.getName().str(),
        .instructions = func_insts,
        .basic_blocks = func_bbs,
    });
  }

  // Sort by instruction count descending
  std::sort(
      func_stats.begin(), func_stats.end(), [](const auto& a, const auto& b) {
        return a.instructions > b.instructions;
      });

  // Print summary
  fmt::print(
      sink, "[lyra][stats] functions={} globals={} bbs={} insts={}\n",
      defined_functions, global_count, total_bbs, total_insts);

  // Print max function (if any defined functions exist)
  if (!func_stats.empty() && total_insts > 0) {
    const auto& max_func = func_stats[0];
    double percent = 100.0 * static_cast<double>(max_func.instructions) /
                     static_cast<double>(total_insts);
    fmt::print(
        sink, "[lyra][stats][max] name={} insts={} percent={:.1f}%\n",
        max_func.name, max_func.instructions, percent);
  }

  // Print top N functions
  int count = std::min(top_n, static_cast<int>(func_stats.size()));
  for (int i = 0; i < count; ++i) {
    const auto& fs = func_stats[i];
    fmt::print(
        sink, "[lyra][stats][top] {}) {} insts={} bbs={}\n", i + 1, fs.name,
        fs.instructions, fs.basic_blocks);
  }

  std::fflush(sink);
}

}  // namespace lyra::driver
