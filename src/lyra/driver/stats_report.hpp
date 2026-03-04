#pragma once

#include <filesystem>
#include <optional>
#include <string>

#include "llvm_stats.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"

namespace lyra::driver {

class VerboseLogger;

enum class StatsBackend { kAot, kJit };

struct StatsReport {
  static constexpr int kVersion = 1;

  StatsBackend backend = StatsBackend::kAot;
  std::string git_sha;
  const VerboseLogger* vlog = nullptr;
  LlvmStats llvm;
  lowering::hir_to_mir::LoweringStats mir;
  std::optional<lowering::mir_to_llvm::JitCompileTimings> jit;
};

// Resolve git short SHA. Returns empty string if not in a git repo.
auto ResolveGitSha() -> std::string;

void WriteStatsJson(
    const StatsReport& report, const std::filesystem::path& path);

}  // namespace lyra::driver
