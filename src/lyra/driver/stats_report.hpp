#pragma once

#include <filesystem>
#include <optional>
#include <string>

#include "llvm_stats.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "output_phase.hpp"

namespace lyra::driver {

enum class StatsBackend { kAot, kJit };

struct StatsReport {
  static constexpr int kVersion = 1;

  StatsBackend backend = StatsBackend::kAot;
  std::string git_sha;
  PhaseSummaryData phases;
  LlvmStats llvm;
  lowering::hir_to_mir::LoweringStats mir;
  std::optional<lowering::mir_to_llvm::JitCompileTimings> jit;
};

auto ResolveGitSha() -> std::string;

void WriteStatsJson(
    const StatsReport& report, const std::filesystem::path& path);

}  // namespace lyra::driver
