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
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/timing_collector.hpp"

namespace lyra::test {

auto RunLliBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult {
  using Clock = std::chrono::steady_clock;
  auto t_total = Clock::now();
  TestResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result = PrepareLlvmModule(test_case, work_directory);
  if (!prep_result) {
    result.error_message = prep_result.error();
    return result;
  }

  // Copy frontend timings
  result.timings.parse = prep_result->parse_seconds;
  result.timings.hir_lower = prep_result->hir_lower_seconds;
  result.timings.mir_lower = prep_result->mir_lower_seconds;
  result.timings.llvm_lower = prep_result->llvm_lower_seconds;

  // Write IR to temp file (counted as "backend")
  auto t_backend = Clock::now();
  auto ir_dir = MakeUniqueTempPath(test_case.name + "_ir");
  std::filesystem::create_directories(ir_dir);
  ScopedTempDirectory ir_guard(ir_dir);

  auto ir_path = ir_dir / "test.ll";
  {
    std::ofstream out(ir_path);
    if (!out) {
      result.error_message = "Failed to write IR file";
      return result;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(prep_result->llvm_result);
  }
  result.timings.backend =
      std::chrono::duration<double>(Clock::now() - t_backend).count();

  // Compile DPI companion C sources into shared objects.
  std::vector<std::filesystem::path> dpi_link_inputs;
  if (!test_case.dpi_sources.empty()) {
    auto dpi = CompileDpiSources(test_case.dpi_sources, work_directory);
    if (!dpi.Ok()) {
      result.error_message = dpi.error;
      return result;
    }
    dpi_link_inputs = std::move(dpi.link_inputs);
  }

  auto runtime_path = FindRuntimeLibrary(runtime::kSharedLibName);
  if (!runtime_path) {
    result.error_message = "Runtime library not found";
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
  auto sub = RunSubprocess("lli", lli_args);
  result.timings.execute =
      std::chrono::duration<double>(Clock::now() - t_exec).count();

  if (sub.exit_code < 0) {
    result.error_message =
        std::format("Failed to run lli: {}", sub.stderr_text);
    return result;
  }

  auto parsed = ParseLyraVarOutput(sub.stdout_text);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.compiler_output = std::move(prep_result->compiler_output);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  result.timings.total =
      std::chrono::duration<double>(Clock::now() - t_total).count();

  if (IsTimingEnabled()) {
    GetTimingCollector().Record(test_case.name, result.timings);
  }

  return result;
}

}  // namespace lyra::test
