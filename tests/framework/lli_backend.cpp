#include "tests/framework/lli_backend.hpp"

#include <array>
#include <cerrno>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <string>
#include <utility>

// POSIX headers
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>

#include "lyra/llvm_backend/lower.hpp"
#include "tests/framework/llvm_common.hpp"
#include "tests/framework/output_protocol.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

// Find the runtime library for LLVM backend
auto FindRuntimeLibrary() -> std::optional<std::filesystem::path> {
  std::filesystem::path exe_path;
  try {
    exe_path = std::filesystem::read_symlink("/proc/self/exe");
  } catch (const std::filesystem::filesystem_error&) {
    return std::nullopt;
  }

  // Try runfiles path (Bazel test environment)
  auto runfiles_path = std::filesystem::path(exe_path.string() + ".runfiles") /
                       "_main" / "liblyra_runtime.so";
  if (std::filesystem::exists(runfiles_path)) {
    return runfiles_path;
  }

  // Try sibling path
  auto sibling_path = exe_path.parent_path() / "liblyra_runtime.so";
  if (std::filesystem::exists(sibling_path)) {
    return sibling_path;
  }

  return std::nullopt;
}

// Run lli subprocess with output capture
auto RunLliWithCapture(
    const std::filesystem::path& runtime_path,
    const std::filesystem::path& ir_path) -> std::pair<int, std::string> {
  std::array<int, 2> stdout_pipe{};
  if (pipe(stdout_pipe.data()) != 0) {
    return {-1, "failed to create pipe"};
  }

  posix_spawn_file_actions_t actions{};
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);

  std::string lli_cmd = "lli";
  std::string dlopen_arg = std::format("--dlopen={}", runtime_path.string());
  std::string ir_path_str = ir_path.string();

  std::array<char*, 4> argv = {
      lli_cmd.data(), dlopen_arg.data(), ir_path_str.data(), nullptr};

  pid_t pid = 0;
  // NOLINTNEXTLINE(misc-include-cleaner) - environ is from unistd.h
  int spawn_result =
      posix_spawnp(&pid, "lli", &actions, nullptr, argv.data(), environ);
  posix_spawn_file_actions_destroy(&actions);

  if (spawn_result != 0) {
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    return {-1, std::format("posix_spawnp failed: {}", std::strerror(errno))};
  }

  close(stdout_pipe[1]);

  std::string output;
  std::array<char, 4096> buffer{};
  ssize_t bytes_read = 0;
  while ((bytes_read = read(stdout_pipe[0], buffer.data(), buffer.size())) >
         0) {
    output.append(buffer.data(), static_cast<size_t>(bytes_read));
  }
  close(stdout_pipe[0]);

  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    return {-1, "waitpid failed"};
  }

  if (WIFEXITED(status)) {
    return {WEXITSTATUS(status), output};
  }
  return {-1, "process did not exit normally"};
}

}  // namespace

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

  auto [exit_code, output] = RunLliWithCapture(*runtime_path, ir_path);
  if (exit_code < 0) {
    result.error_message = std::format("Failed to run lli: {}", output);
    return result;
  }

  auto parsed = ParseLyraVarOutput(output);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  return result;
}

}  // namespace lyra::test
