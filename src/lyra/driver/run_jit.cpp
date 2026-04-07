#include "run_jit.hpp"

#include <cstdio>
#include <format>
#include <string>
#include <unistd.h>
#include <utility>
#include <vector>

#include <llvm/Support/Error.h>
#include <llvm/Support/TimeProfiler.h>

#include "compilation_output.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/runtime/artifact_names.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "pipeline.hpp"
#include "process_stats.hpp"
#include "runtime_path.hpp"
#include "stats_report.hpp"
#include "validated_input.hpp"

namespace lyra::driver {

namespace {

// RAII guard for LLVM time-trace profiling.
class TimeTraceGuard {
 public:
  TimeTraceGuard(bool enabled, CompilationOutput& output)
      : enabled_(enabled), output_(output) {
    if (!enabled_) return;
    llvm::timeTraceProfilerInitialize(500, "lyra");
    filename_ =
        std::format("lyra-jit-{}.time-trace.json", static_cast<int>(getpid()));
  }

  ~TimeTraceGuard() {
    if (!enabled_) return;
    if (auto err = llvm::timeTraceProfilerWrite(filename_, "lyra")) {
      output_.PrintWarning(
          std::format(
              "failed to write time-trace: {}",
              llvm::toString(std::move(err))));
    } else {
      output_.PrintWarning(std::format("time-trace written to {}", filename_));
    }
    llvm::timeTraceProfilerCleanup();
  }

  TimeTraceGuard(const TimeTraceGuard&) = delete;
  auto operator=(const TimeTraceGuard&) -> TimeTraceGuard& = delete;
  TimeTraceGuard(TimeTraceGuard&&) = delete;
  auto operator=(TimeTraceGuard&&) -> TimeTraceGuard& = delete;

 private:
  bool enabled_;
  CompilationOutput& output_;
  std::string filename_;
};

}  // namespace

auto RunJit(const ValidatedCompilationInput& input) -> int {
  bool emit_stats = input.input.stats_top_n >= 0;
  CompilationOutput output(
      BuildJitDriverOutputOptions(input.input, emit_stats));

  auto result = CompileToMir(input.input, output);
  if (!result) {
    result.error().Render(output);
    output.Flush();
    return 1;
  }
  auto compilation = std::move(*result);

  lowering::OriginMapLookup origin_lookup(
      &compilation.mir.design_origins, &compilation.mir.body_origins,
      &compilation.hir.design, compilation.hir.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  uint32_t feature_flags = 0;
  if (input.input.enable_trace_summary) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableTraceSummary);
  }
  if (input.input.trace_signals_output.has_value()) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableSignalTrace);
  }
  if (input.input.enable_system) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableSystem);
  }
  if (input.input.trace_activations) {
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableActivationTrace);
  }
  if (input.input.verbose >= 2) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDumpRuntimeStats);
  }
  if (input.input.verbose >= 3) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDetailedStats);
  }
  if (input.input.dump_suspended) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDumpSuspended);
  }

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation.mir.design,
      .construction = &compilation.mir.construction,
      .mir_arena = compilation.mir.design_arena.get(),
      .type_arena = compilation.hir.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .source_manager = compilation.hir.source_manager.get(),
      .origin_lookup = &origin_lookup,
      .fs_base_dir = input.input.fs_base_dir.string(),
      .plusargs = input.input.plusargs,
      .feature_flags = feature_flags,
      .signal_trace_path = input.input.trace_signals_output.value_or(""),
      .iteration_limit = input.input.iteration_limit,
      .body_timescales = &compilation.hir.body_timescales,
      .force_two_state = input.input.two_state,
      .collect_forwarding_analysis =
          output.IsEnabled(OutputCategory::kAnalysis),
      .dpi_export_wrappers = &compilation.mir.dpi_export_wrappers,
      .resolved_bindings = &compilation.mir.resolved_bindings,
      .bound_connections = &compilation.mir.bound_connections,
  };

  std::expected<lowering::mir_to_llvm::LoweringResult, Diagnostic> llvm_result;
  {
    PhaseTimer timer(output, Phase::kLowerLlvm);
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  }
  if (!llvm_result) {
    output.PrintDiagnostic(
        llvm_result.error(), *compilation.hir.source_manager);
    output.Flush();
    return 1;
  }

  std::vector<std::string> tried_paths;
  auto runtime_path = FindRuntimeLibrary(runtime::kSharedLibName, tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    output.PrintError(msg);
    output.Flush();
    return 1;
  }

  bool collect_stats = emit_stats || input.input.stats_out_path.has_value();
  LlvmStats llvm_stats;
  if (collect_stats) {
    llvm_stats = CollectLlvmStats(*llvm_result->module);
  }

  if (emit_stats) {
    output.PrintMirStats(compilation.mir.stats);
    output.PrintPhaseSummary();
    output.PrintLlvmStats(llvm_stats, input.input.stats_top_n);
    auto ps = CollectProcessStats(
        compilation.mir.design, *compilation.mir.design_arena,
        compilation.mir.design_origins, compilation.hir.design,
        *compilation.hir.hir_arena, *compilation.hir.source_manager,
        llvm_stats);
    output.PrintProcessStats(ps);
  }

  if (output.IsEnabled(OutputCategory::kAnalysis)) {
    output.PrintForwardingAnalysisReport(
        llvm_result->report.forwarding_analysis);
  }

  TimeTraceGuard time_trace_guard(input.input.time_trace, output);
  std::expected<lowering::mir_to_llvm::JitSession, std::string> session;
  {
    PhaseTimer timer(output, Phase::kJitCompile);
    lowering::mir_to_llvm::LinkProgressReporter progress_reporter;
    if (output.IsEnabled(OutputCategory::kLinkProgress)) {
      progress_reporter =
          [&output](
              const lowering::mir_to_llvm::LinkProgressSnapshot& snapshot) {
            output.PrintLinkProgress(snapshot);
          };
    }
    lowering::mir_to_llvm::JitCompileOptions jit_opts{
        .opt_level = input.input.opt_level,
        .enable_profiling = emit_stats,
        .progress_reporter = std::move(progress_reporter),
        .dpi_link_inputs = input.input.dpi_link_inputs,
        .dpi_object_inputs = {},
    };
    session =
        lowering::mir_to_llvm::CompileJit(*llvm_result, runtime_path, jit_opts);
  }
  if (!session) {
    output.PrintError(
        std::format("JIT compilation failed: {}", session.error()));
    output.Flush();
    return 1;
  }

  if (emit_stats) {
    output.PrintJitTimings(session->Timings());
    output.PrintOrcStats(session->OrcStats());
  }

  if (input.input.stats_out_path) {
    StatsReport report{
        .backend = StatsBackend::kJit,
        .git_sha = ResolveGitSha(),
        .phases = output.GetPhaseSummaryData(),
        .llvm = llvm_stats,
        .mir = compilation.mir.stats,
        .jit = session->Timings(),
    };
    WriteStatsJson(report, *input.input.stats_out_path);
  }

  int exit_code = 0;
  {
    PhaseTimer timer(output, Phase::kSim, HeartbeatPolicy::kEnabled);
    exit_code = session->Run();
  }
  output.Flush();
  return exit_code;
}

}  // namespace lyra::driver
