#include "compilation_output.hpp"

#include <string_view>

#include <fmt/core.h>

#include "lyra/common/internal_error.hpp"
#include "render_analysis.hpp"
#include "render_diagnostics.hpp"
#include "render_link_progress.hpp"
#include "render_stats.hpp"
#include "text_sink.hpp"

namespace lyra::driver {

namespace {

auto PhaseName(Phase phase) -> std::string_view {
  for (const auto& [p, name] : kOrderedPhases) {
    if (p == phase) return name;
  }
  throw common::InternalError("PhaseName", "invalid phase");
}

}  // namespace

CompilationOutput::CompilationOutput(const DriverOutputOptions& options)
    : CompilationOutput(options, CreateStderrSink()) {
}

CompilationOutput::CompilationOutput(
    const DriverOutputOptions& options, std::unique_ptr<TextSink> sink)
    : options_(options), sink_(std::move(sink)) {
  if (!sink_) {
    throw common::InternalError("CompilationOutput", "sink must not be null");
  }
}

CompilationOutput::~CompilationOutput() = default;
CompilationOutput::CompilationOutput(CompilationOutput&&) noexcept = default;
auto CompilationOutput::operator=(CompilationOutput&&) noexcept
    -> CompilationOutput& = default;

auto CompilationOutput::IsEnabled(OutputCategory category) const -> bool {
  switch (category) {
    case OutputCategory::kPhaseProgress:
      return options_.show_phase_progress;
    case OutputCategory::kStats:
      return options_.show_stats;
    case OutputCategory::kAnalysis:
      return options_.show_analysis;
    case OutputCategory::kLinkProgress:
      return options_.show_link_progress;
  }
  throw common::InternalError(
      "CompilationOutput::IsEnabled", "invalid output category");
}

void CompilationOutput::PhaseBegin(Phase phase) {
  if (!IsEnabled(OutputCategory::kPhaseProgress)) return;
  auto name = PhaseName(phase);
  sink_->Write(fmt::format("[lyra][phase] {}: begin\n", name));
  sink_->Flush();
}

void CompilationOutput::PhaseDone(Phase phase, double seconds) {
  if (!IsEnabled(OutputCategory::kPhaseProgress)) return;
  auto name = PhaseName(phase);
  sink_->Write(
      fmt::format("[lyra][phase] {}: done ({:.2f}s)\n", name, seconds));
  sink_->Flush();
}

void CompilationOutput::Progress(Phase phase, double elapsed_seconds) {
  if (!IsEnabled(OutputCategory::kPhaseProgress)) return;
  auto name = PhaseName(phase);
  sink_->Write(
      fmt::format(
          "[lyra][progress] {}: still running ({:.0f}s)...\n", name,
          elapsed_seconds));
  sink_->Flush();
}

void CompilationOutput::RecordPhaseDuration(Phase phase, double seconds) {
  auto idx = static_cast<size_t>(phase);
  if (idx < kPhaseCount) {
    phase_durations_[idx] = seconds;
    phase_recorded_[idx] = true;
  }
}

auto CompilationOutput::PhaseDuration(Phase phase) const
    -> std::optional<double> {
  auto idx = static_cast<size_t>(phase);
  if (idx < kPhaseCount && phase_recorded_[idx]) {
    return phase_durations_[idx];
  }
  return std::nullopt;
}

auto CompilationOutput::GetPhaseSummaryData() const -> PhaseSummaryData {
  return PhaseSummaryData{
      .durations = phase_durations_,
      .recorded = phase_recorded_,
  };
}

void CompilationOutput::FlushIfNeeded(FlushPolicy policy) const {
  if (policy == FlushPolicy::kImmediate) {
    sink_->Flush();
  }
}

void CompilationOutput::PrintDiagnostic(const Diagnostic& diag) const {
  RenderDiagnostic(*sink_, diag, nullptr);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintDiagnostic(
    const Diagnostic& diag, const SourceManager& source_manager) const {
  RenderDiagnostic(*sink_, diag, &source_manager);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintDiagnostics(
    const DiagnosticSink& diag_sink,
    const SourceManager* source_manager) const {
  RenderDiagnostics(*sink_, diag_sink, source_manager);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintError(std::string_view message) const {
  RenderError(*sink_, message);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintWarning(std::string_view message) const {
  RenderWarning(*sink_, message);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintInternalError(
    std::string_view context_fn, std::string_view message) const {
  RenderInternalError(*sink_, context_fn, message);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintPhaseSummary() const {
  RenderPhaseSummary(*sink_, phase_durations_, phase_recorded_);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintMirStats(
    const lowering::hir_to_mir::LoweringStats& stats) const {
  RenderMirStats(*sink_, stats);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintLlvmStats(
    const LlvmStats& stats, int top_n) const {
  RenderLlvmStats(*sink_, stats, top_n);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintProcessStats(const ProcessStatsData& data) const {
  RenderProcessStats(*sink_, data);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintJitTimings(
    const lowering::mir_to_llvm::JitCompileTimings& timings) const {
  RenderJitTimings(*sink_, timings);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintOrcStats(
    const lowering::mir_to_llvm::JitOrcStats& stats) const {
  RenderOrcStats(*sink_, stats);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::PrintForwardingAnalysisReport(
    const lowering::mir_to_llvm::ForwardingAnalysisReport& report) const {
  RenderForwardingAnalysisReport(*sink_, report);
  FlushIfNeeded(FlushPolicy::kDeferred);
}

void CompilationOutput::PrintLinkProgress(
    const lowering::mir_to_llvm::LinkProgressSnapshot& snapshot) const {
  RenderLinkProgress(*sink_, snapshot);
  FlushIfNeeded(FlushPolicy::kImmediate);
}

void CompilationOutput::Flush() const {
  sink_->Flush();
}

PhaseTimer::PhaseTimer(
    CompilationOutput& output, Phase phase, HeartbeatPolicy heartbeat)
    : output_(output),
      phase_(phase),
      start_(std::chrono::steady_clock::now()),
      enabled_(output.IsEnabled(OutputCategory::kPhaseProgress)),
      heartbeat_enabled_(heartbeat == HeartbeatPolicy::kEnabled && enabled_) {
  if (enabled_) {
    output_.PhaseBegin(phase_);
  }
  if (heartbeat_enabled_) {
    heartbeat_thread_ = std::jthread(
        [this](std::stop_token st) { HeartbeatLoop(std::move(st)); });
  }
}

PhaseTimer::~PhaseTimer() {
  if (heartbeat_enabled_) {
    heartbeat_thread_.request_stop();
    heartbeat_cv_.notify_all();
  }

  auto end = std::chrono::steady_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::milliseconds>(end - start_);
  double seconds = duration.count() / 1000.0;

  output_.RecordPhaseDuration(phase_, seconds);

  if (enabled_) {
    output_.PhaseDone(phase_, seconds);
  }
}

void PhaseTimer::HeartbeatLoop(std::stop_token stop_token) {
  constexpr auto kThreshold = std::chrono::seconds(10);
  constexpr auto kInterval = std::chrono::seconds(10);

  {
    std::unique_lock lock(heartbeat_mutex_);
    if (heartbeat_cv_.wait_for(lock, stop_token, kThreshold, [&] {
          return stop_token.stop_requested();
        })) {
      return;
    }
  }

  while (!stop_token.stop_requested()) {
    auto elapsed = std::chrono::steady_clock::now() - start_;
    auto elapsed_secs =
        std::chrono::duration_cast<std::chrono::seconds>(elapsed).count();
    output_.Progress(phase_, static_cast<double>(elapsed_secs));

    std::unique_lock lock(heartbeat_mutex_);
    if (heartbeat_cv_.wait_for(lock, stop_token, kInterval, [&] {
          return stop_token.stop_requested();
        })) {
      return;
    }
  }
}

}  // namespace lyra::driver
