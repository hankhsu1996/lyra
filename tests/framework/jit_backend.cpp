#include "tests/framework/jit_backend.hpp"

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

namespace lyra::test {

auto RunJitBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult {
  TestResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result = PrepareLlvmModule(test_case, work_directory);
  if (!prep_result) {
    result.error_message = prep_result.error();
    return result;
  }

  // Execute using in-process ORC JIT with host process symbols.
  // This ensures JIT code uses the same runtime globals as the test framework.
  // OutputSinkScope captures JIT output and restores previous sink on exit.
  std::string captured_output;
  std::expected<int, std::string> exec_result;
  {
    runtime::OutputSinkScope sink_scope(
        [&captured_output](std::string_view text) {
          captured_output.append(text);
        });
    exec_result = lowering::mir_to_llvm::ExecuteWithOrcJitInProcess(
        prep_result->llvm_result, OptLevel::kO0);
  }

  if (!exec_result) {
    result.error_message =
        std::format("JIT execution failed: {}", exec_result.error());
    return result;
  }

  // Parse output to extract variables and time
  auto parsed = ParseLyraVarOutput(captured_output);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  return result;
}

}  // namespace lyra::test
