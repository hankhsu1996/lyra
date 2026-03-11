#include "tests/framework/aot_backend.hpp"

#include <cstdlib>
#include <filesystem>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/emit.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

auto RunAotBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult {
  TestResult result;

  // Prepare LLVM module (AST -> HIR -> MIR -> LLVM)
  auto prep_result = PrepareLlvmModule(test_case, work_directory);
  if (!prep_result) {
    result.error_message = prep_result.error();
    return result;
  }

  // Create temp directory for AOT artifacts
  auto aot_dir = MakeUniqueTempPath(test_case.name + "_aot");
  std::filesystem::create_directories(aot_dir);
  ScopedTempDirectory aot_guard(aot_dir);

  // Emit object file
  auto target_machine =
      lowering::mir_to_llvm::CreateHostTargetMachine(OptLevel::kO0);
  auto obj_path = aot_dir / "test.o";
  auto emit_result = lowering::mir_to_llvm::EmitObjectFile(
      *prep_result->llvm_result.module, *target_machine, obj_path);
  if (!emit_result) {
    result.error_message =
        std::format("Object emission failed: {}", emit_result.error());
    return result;
  }

  // Link test executable against the shared runtime (fast dynamic link).
  // Uses the shared library for test speed; production `lyra compile` uses
  // the static archive for self-contained output.
  auto link_result = LinkTestExecutable(obj_path, aot_dir, "test");
  if (!link_result) {
    result.error_message =
        std::format("Linking failed: {}", link_result.error());
    return result;
  }

  // Execute the linked binary with LD_LIBRARY_PATH prepended to include the
  // shared runtime directory. The binary is ephemeral -- no rpath or bundle
  // needed.
  std::vector<std::string> no_args;
  std::string ld_path = link_result->runtime_dir.string();
  const char* existing = std::getenv("LD_LIBRARY_PATH");
  if (existing != nullptr && existing[0] != '\0') {
    ld_path += ':';
    ld_path += existing;
  }
  EnvOverrides env = {{"LD_LIBRARY_PATH", ld_path}};
  auto sub = RunSubprocess(link_result->exe_path, no_args, env);
  if (sub.exit_code < 0) {
    result.error_message =
        std::format("AOT execution failed: {}", sub.stderr_text);
    return result;
  }
  if (sub.exit_code != 0) {
    result.error_message = std::format(
        "AOT executable exited with code {} (stderr: {})", sub.exit_code,
        sub.stderr_text);
    return result;
  }

  auto parsed = ParseLyraVarOutput(sub.stdout_text);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.compiler_output = std::move(prep_result->compiler_output);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  return result;
}

}  // namespace lyra::test
