#pragma once

#include <chrono>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <optional>
#include <string_view>
#include <thread>

#include "driver_output_types.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_manager.hpp"
#include "output_phase.hpp"

namespace lyra::lowering::hir_to_mir {
struct LoweringStats;
}

namespace lyra::lowering::mir_to_llvm {
struct JitCompileTimings;
struct JitOrcStats;
struct LinkProgressSnapshot;
class ForwardingAnalysisReport;
}  // namespace lyra::lowering::mir_to_llvm

namespace lyra::driver {

class TextSink;
struct LlvmStats;
struct ProcessStatsData;
enum class OutputCategory : uint8_t {
  kPhaseProgress,
  kStats,
  kAnalysis,
  kLinkProgress,
};

enum class FlushPolicy : uint8_t {
  kImmediate,
  kDeferred,
};

class CompilationOutput {
 public:
  explicit CompilationOutput(const DriverOutputOptions& options);
  CompilationOutput(
      const DriverOutputOptions& options, std::unique_ptr<TextSink> sink);
  ~CompilationOutput();

  CompilationOutput(const CompilationOutput&) = delete;
  auto operator=(const CompilationOutput&) -> CompilationOutput& = delete;
  CompilationOutput(CompilationOutput&&) noexcept;
  auto operator=(CompilationOutput&&) noexcept -> CompilationOutput&;

  [[nodiscard]] auto IsEnabled(OutputCategory category) const -> bool;

  // Phase timing.
  void PhaseBegin(Phase phase);
  void PhaseDone(Phase phase, double seconds);
  void Progress(Phase phase, double elapsed_seconds);
  void RecordPhaseDuration(Phase phase, double seconds);
  [[nodiscard]] auto PhaseDuration(Phase phase) const -> std::optional<double>;
  [[nodiscard]] auto GetPhaseSummaryData() const -> PhaseSummaryData;

  // Diagnostics and errors.
  void PrintDiagnostic(const Diagnostic& diag) const;
  void PrintDiagnostic(
      const Diagnostic& diag, const SourceManager& source_manager) const;
  void PrintDiagnostics(
      const DiagnosticSink& sink,
      const SourceManager* source_manager = nullptr) const;
  void PrintError(std::string_view message) const;
  void PrintWarning(std::string_view message) const;
  void PrintInternalError(
      std::string_view context_fn, std::string_view message) const;

  // Stats rendering.
  void PrintPhaseSummary() const;
  void PrintMirStats(const lowering::hir_to_mir::LoweringStats& stats) const;
  void PrintLlvmStats(const LlvmStats& stats, int top_n) const;
  void PrintProcessStats(const ProcessStatsData& data) const;
  void PrintJitTimings(
      const lowering::mir_to_llvm::JitCompileTimings& timings) const;
  void PrintOrcStats(const lowering::mir_to_llvm::JitOrcStats& stats) const;

  // Analysis rendering.
  void PrintForwardingAnalysisReport(
      const lowering::mir_to_llvm::ForwardingAnalysisReport& report) const;

  // JIT link progress rendering.
  void PrintLinkProgress(
      const lowering::mir_to_llvm::LinkProgressSnapshot& snapshot) const;

  // Explicit flush for deferred output.
  void Flush() const;

 private:
  void FlushIfNeeded(FlushPolicy policy) const;

  DriverOutputOptions options_;
  std::unique_ptr<TextSink> sink_;
  PhaseDurationArray phase_durations_{};
  PhaseRecordedArray phase_recorded_{};
};

class PhaseTimer {
 public:
  PhaseTimer(
      CompilationOutput& output, Phase phase,
      HeartbeatPolicy heartbeat = HeartbeatPolicy::kDisabled);
  ~PhaseTimer();

  PhaseTimer(const PhaseTimer&) = delete;
  auto operator=(const PhaseTimer&) -> PhaseTimer& = delete;
  PhaseTimer(PhaseTimer&&) = delete;
  auto operator=(PhaseTimer&&) -> PhaseTimer& = delete;

 private:
  void HeartbeatLoop(std::stop_token stop_token);

  CompilationOutput& output_;
  Phase phase_;
  std::chrono::steady_clock::time_point start_;
  bool enabled_;

  bool heartbeat_enabled_;
  std::jthread heartbeat_thread_;
  std::mutex heartbeat_mutex_;
  std::condition_variable_any heartbeat_cv_;
};

}  // namespace lyra::driver
