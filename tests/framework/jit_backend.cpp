#include "tests/framework/jit_backend.hpp"

#include <chrono>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/runtime/cover_hook.hpp"
#include "lyra/runtime/nba_stats_hook.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "tests/framework/dpi_test_support.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/timing_collector.hpp"

namespace lyra::test {

auto RunJitBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state) -> CaseExecutionResult {
  using Clock = std::chrono::steady_clock;
  auto t_total = Clock::now();
  CaseExecutionResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result =
      PrepareLlvmModule(test_case, work_directory, force_two_state);
  if (!prep_result) {
    result.execution.outcome = ExecutionOutcome::kFrontendError;
    result.execution.error_message = prep_result.error();
    return result;
  }

  // Copy frontend timings
  result.artifacts.timings.parse = prep_result->parse_seconds;
  result.artifacts.timings.hir_lower = prep_result->hir_lower_seconds;
  result.artifacts.timings.mir_lower = prep_result->mir_lower_seconds;
  result.artifacts.timings.llvm_lower = prep_result->llvm_lower_seconds;

  // Compile DPI companion C sources into object files.
  // Object files are loaded directly into the JIT's symbol space,
  // enabling bidirectional symbol resolution (both import and export).
  std::vector<std::filesystem::path> dpi_object_inputs;
  if (!test_case.dpi_sources.empty()) {
    auto dpi =
        CompileDpiSourcesToObjects(test_case.dpi_sources, work_directory);
    if (!dpi.Ok()) {
      result.execution.outcome = ExecutionOutcome::kBackendSetupError;
      result.execution.error_message = dpi.error;
      return result;
    }
    dpi_object_inputs = std::move(dpi.link_inputs);
  }

  // Compile using in-process ORC JIT with host process symbols.
  // DPI object files are added directly to the JIT symbol space.
  std::string captured_output;
  NbaRoutingStatsResult nba_stats;
  auto t_backend = Clock::now();
  lowering::mir_to_llvm::JitCompileOptions jit_opts{
      .opt_level = OptLevel::kO0,
      .enable_profiling = false,
      .progress_reporter = nullptr,
      .dpi_link_inputs = {},
      .dpi_object_inputs = std::move(dpi_object_inputs),
  };
  auto session = lowering::mir_to_llvm::CompileJitInProcess(
      prep_result->llvm_result, jit_opts);
  result.artifacts.timings.backend =
      std::chrono::duration<double>(Clock::now() - t_backend).count();

  if (!session) {
    result.execution.outcome = ExecutionOutcome::kBackendSetupError;
    result.execution.error_message =
        std::format("JIT compilation failed: {}", session.error());
    return result;
  }

  // Execute the compiled simulation.
  // OutputSinkScope captures JIT output and restores previous sink on exit.
  // CoverHitCallbackScope captures per-site cover hit counts.
  auto t_exec = Clock::now();
  int exit_code = 0;
  std::vector<uint64_t> cover_hits;
  {
    runtime::OutputSinkScope sink_scope(
        [&captured_output](std::string_view text) {
          captured_output.append(text);
        });
    runtime::CoverHitCallbackScope cover_scope(
        [&cover_hits](std::vector<uint64_t> hits) {
          cover_hits = std::move(hits);
        });
    runtime::NbaStatsCallbackScope nba_scope(
        [&nba_stats](runtime::NbaRoutingStats stats) {
          nba_stats.generic_queue = stats.generic_queue;
          nba_stats.deferred_local = stats.deferred_local;
          nba_stats.captured = true;
        });
    exit_code = session->Run();
  }
  result.artifacts.timings.execute =
      std::chrono::duration<double>(Clock::now() - t_exec).count();

  if (exit_code != 0) {
    result.execution.outcome = ExecutionOutcome::kExecutionFailed;
    result.execution.error_message =
        std::format("JIT execution returned non-zero: {}", exit_code);
    result.execution.exit_code = exit_code;
    return result;
  }

  // Parse output to extract variables and time
  auto parsed = ParseLyraVarOutput(captured_output);
  result.execution.outcome = ExecutionOutcome::kSuccess;
  result.artifacts.captured_output = std::move(parsed.clean);
  result.artifacts.compiler_output = std::move(prep_result->compiler_output);
  result.artifacts.variables = std::move(parsed.variables);
  result.artifacts.final_time = parsed.final_time;
  result.artifacts.cover_hits = std::move(cover_hits);
  result.artifacts.nba_stats = nba_stats;
  result.artifacts.timings.total =
      std::chrono::duration<double>(Clock::now() - t_total).count();

  if (IsTimingEnabled()) {
    GetTimingCollector().Record(test_case.name, result.artifacts.timings);
  }

  return result;
}

}  // namespace lyra::test
