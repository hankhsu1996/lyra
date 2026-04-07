#include "tests/framework/aot_backend.hpp"

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/emit.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "tests/framework/dpi_test_support.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/process_runner.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/timing_collector.hpp"

namespace lyra::test {

auto RunAotBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> CaseExecutionResult {
  using Clock = std::chrono::steady_clock;
  auto t_total = Clock::now();
  CaseExecutionResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result = PrepareLlvmModule(test_case, work_directory);
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

  // Create temp directory for AOT artifacts
  auto aot_dir = MakeUniqueTempPath(test_case.name + "_aot");
  std::filesystem::create_directories(aot_dir);
  ScopedTempDirectory aot_guard(aot_dir);

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

  // Emit object file + link (both counted as "backend")
  auto t_backend = Clock::now();
  auto target_machine =
      lowering::mir_to_llvm::CreateHostTargetMachine(OptLevel::kO0);
  auto obj_path = aot_dir / "test.o";
  auto emit_result = lowering::mir_to_llvm::EmitObjectFile(
      *prep_result->llvm_result.module, *target_machine, obj_path);
  if (!emit_result) {
    result.execution.outcome = ExecutionOutcome::kBackendSetupError;
    result.execution.error_message =
        std::format("Object emission failed: {}", emit_result.error());
    return result;
  }

  auto link_result =
      LinkTestExecutable(obj_path, aot_dir, "test", dpi_link_inputs);
  if (!link_result) {
    result.execution.outcome = ExecutionOutcome::kBackendSetupError;
    result.execution.error_message =
        std::format("Linking failed: {}", link_result.error());
    return result;
  }
  result.artifacts.timings.backend =
      std::chrono::duration<double>(Clock::now() - t_backend).count();

  // Execute the linked binary with LD_LIBRARY_PATH prepended to include the
  // shared runtime directory. The binary is ephemeral -- no rpath or bundle
  // needed.
  auto t_exec = Clock::now();
  // Build deduplicated LD_LIBRARY_PATH: runtime dir + DPI link input dirs.
  std::vector<std::filesystem::path> lib_dirs;
  lib_dirs.push_back(link_result->runtime_dir);
  for (const auto& dpi_input : dpi_link_inputs) {
    auto dir = dpi_input.parent_path();
    if (std::ranges::find(lib_dirs, dir) == lib_dirs.end()) {
      lib_dirs.push_back(std::move(dir));
    }
  }

  std::string ld_path;
  for (size_t i = 0; i < lib_dirs.size(); ++i) {
    if (i != 0) ld_path += ':';
    ld_path += lib_dirs[i].string();
  }
  const char* existing = std::getenv("LD_LIBRARY_PATH");
  if (existing != nullptr && std::strlen(existing) > 0) {
    ld_path += ':';
    ld_path += existing;
  }

  std::vector<std::string> no_args;
  EnvOverrides env = {{"LD_LIBRARY_PATH", ld_path}};
  auto proc = RunChildProcess(link_result->exe_path, no_args, env);
  result.artifacts.timings.execute =
      std::chrono::duration<double>(Clock::now() - t_exec).count();

  if (proc.termination != TerminationKind::kExitedNormally) {
    result.execution =
        MapProcessOutcomeToExecutionResult("AOT executable", proc);
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
