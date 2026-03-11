#include "tests/framework/jit_backend.hpp"

#include <chrono>
#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <utility>

#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/timing_collector.hpp"

namespace lyra::test {

auto RunJitBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state) -> TestResult {
  using Clock = std::chrono::steady_clock;
  auto t_total = Clock::now();
  TestResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result =
      PrepareLlvmModule(test_case, work_directory, force_two_state);
  if (!prep_result) {
    result.error_message = prep_result.error();
    return result;
  }

  // Copy frontend timings
  result.timings.parse = prep_result->parse_seconds;
  result.timings.hir_lower = prep_result->hir_lower_seconds;
  result.timings.mir_lower = prep_result->mir_lower_seconds;
  result.timings.llvm_lower = prep_result->llvm_lower_seconds;

  // Compile using in-process ORC JIT with host process symbols.
  std::string captured_output;
  auto t_backend = Clock::now();
  lowering::mir_to_llvm::JitCompileOptions jit_opts{
      .opt_level = OptLevel::kO0,
  };
  auto session = lowering::mir_to_llvm::CompileJitInProcess(
      prep_result->llvm_result, jit_opts);
  result.timings.backend =
      std::chrono::duration<double>(Clock::now() - t_backend).count();

  if (!session) {
    result.error_message =
        std::format("JIT compilation failed: {}", session.error());
    return result;
  }

  // Execute the compiled simulation.
  // OutputSinkScope captures JIT output and restores previous sink on exit.
  auto t_exec = Clock::now();
  int exit_code = 0;
  {
    runtime::OutputSinkScope sink_scope(
        [&captured_output](std::string_view text) {
          captured_output.append(text);
        });
    exit_code = session->Run();
  }
  result.timings.execute =
      std::chrono::duration<double>(Clock::now() - t_exec).count();

  if (exit_code != 0) {
    result.error_message =
        std::format("JIT execution returned non-zero: {}", exit_code);
    return result;
  }

  // Parse output to extract variables and time
  auto parsed = ParseLyraVarOutput(captured_output);
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
