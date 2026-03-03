#include "tests/framework/lli_backend.hpp"

#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <utility>
#include <vector>

#include "lyra/llvm_backend/lower.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

auto RunLliBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult {
  TestResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result = PrepareLlvmModule(test_case, work_directory);
  if (!prep_result) {
    result.error_message = prep_result.error();
    return result;
  }

  // Write IR to temp file
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

  auto runtime_path = FindRuntimeLibrary();
  if (!runtime_path) {
    result.error_message = "Runtime library not found";
    return result;
  }

  std::vector<std::string> lli_args = {
      std::format("--dlopen={}", runtime_path->string()),
      ir_path.string(),
  };
  auto sub = RunSubprocess("lli", lli_args);
  if (sub.exit_code < 0) {
    result.error_message =
        std::format("Failed to run lli: {}", sub.stderr_text);
    return result;
  }

  auto parsed = ParseLyraVarOutput(sub.stdout_text);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  return result;
}

}  // namespace lyra::test
