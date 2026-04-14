#include "tests/framework/lli_backend.hpp"

#include <chrono>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <utility>
#include <vector>

#include "lyra/llvm_backend/lower.hpp"
#include "lyra/runtime/artifact_names.hpp"
#include "tests/framework/dpi_test_support.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/process_runner.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/timing_collector.hpp"

namespace lyra::test {

auto RunLliBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    std::chrono::seconds timeout) -> CaseExecutionResult {
  using Clock = std::chrono::steady_clock;
  auto t_total = Clock::now();
  CaseExecutionResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result = PrepareLlvmModule(
      test_case, work_directory, false,
      lowering::mir_to_llvm::MainAbi::kArgvForwarding);
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

  // Write IR to temp file (counted as "backend")
  auto t_backend = Clock::now();
  auto ir_dir = MakeUniqueTempPath(test_case.name + "_ir");
  std::filesystem::create_directories(ir_dir);
  ScopedTempDirectory ir_guard(ir_dir);

  auto ir_path = ir_dir / "test.ll";
  {
    std::ofstream out(ir_path);
    if (!out) {
      result.execution.outcome = ExecutionOutcome::kInfraError;
      result.execution.error_message = "Failed to write IR file";
      return result;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(*prep_result->llvm_result.module);
  }
  result.artifacts.timings.backend =
      std::chrono::duration<double>(Clock::now() - t_backend).count();

  // Compile DPI companion C sources into shared objects.
  std::vector<std::filesystem::path> dpi_link_inputs;
  if (!test_case.dpi_sources.empty()) {
    auto dpi = CompileDpiSources(test_case.dpi_sources, work_directory);
    if (!dpi.Ok()) {
      result.execution.outcome = ExecutionOutcome::kBackendSetupError;
      result.execution.error_message = dpi.error;
      return result;
    }
    dpi_link_inputs = std::move(dpi.link_inputs);
  }

  auto runtime_path = FindRuntimeLibrary(runtime::kSharedLibName);
  if (!runtime_path) {
    result.execution.outcome = ExecutionOutcome::kInfraError;
    result.execution.error_message = "Runtime library not found";
    return result;
  }

  // Execute via lli subprocess with --dlopen for runtime and DPI inputs.
  auto t_exec = Clock::now();
  std::vector<std::string> lli_args = {
      std::format("--dlopen={}", runtime_path->string()),
  };
  for (const auto& dpi : dpi_link_inputs) {
    lli_args.push_back(std::format("--dlopen={}", dpi.string()));
  }
  lli_args.push_back(ir_path.string());
  auto proc = RunChildProcess("lli", lli_args, {}, timeout);
  result.artifacts.timings.execute =
      std::chrono::duration<double>(Clock::now() - t_exec).count();

  // FIX: Previously nonzero exit fell through to success.
  // Now all non-normal terminations are correctly classified.
  if (proc.termination != TerminationKind::kExitedNormally) {
    result.execution = MapProcessOutcomeToExecutionResult("LLI", proc);
    return result;
  }

  auto parsed = ParseLyraVarOutput(proc.stdout_text);
  result.execution.outcome = ExecutionOutcome::kSuccess;
  result.artifacts.captured_output = std::move(parsed.clean);
  result.artifacts.compiler_output = std::move(prep_result->compiler_output);
  result.artifacts.variables = std::move(parsed.variables);
  result.artifacts.final_time = parsed.final_time;
  result.artifacts.timings.total =
      std::chrono::duration<double>(Clock::now() - t_total).count();

  if (IsTimingEnabled()) {
    GetTimingCollector().Record(test_case.name, result.artifacts.timings);
  }

  return result;
}

}  // namespace lyra::test
