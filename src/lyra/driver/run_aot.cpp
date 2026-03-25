#include "run_aot.hpp"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <format>
#include <spawn.h>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

#include "compilation_output.hpp"
#include "compile.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"

namespace lyra::driver {

namespace {

namespace fs = std::filesystem;

auto GetTempDir() -> fs::path {
  return fs::temp_directory_path() / std::format("lyra_aot_{}", getpid());
}

}  // namespace

auto RunAot(const CompilationInput& input) -> int {
  CompilationOutput output(BuildCompileDriverOutputOptions(input));

  auto temp_dir = GetTempDir();

  std::string exe_name = input.top.empty() ? "simulation" : input.top;

  CompileOptions options{
      .output_dir = temp_dir,
      .name = exe_name,
  };

  auto compile_result = Compile(input, options);
  if (!compile_result) {
    fs::remove_all(temp_dir);
    return compile_result.error();
  }

  auto exe_path = *compile_result;

  std::vector<std::string> args;
  args.push_back(exe_path.string());
  for (const auto& plusarg : input.plusargs) {
    args.push_back(plusarg);
  }

  std::vector<char*> argv;
  argv.reserve(args.size() + 1);
  for (auto& arg : args) {
    argv.push_back(arg.data());
  }
  argv.push_back(nullptr);

  setenv("LYRA_FS_BASE_DIR", input.fs_base_dir.string().c_str(), 1);

  pid_t pid = 0;
  int spawn_result = posix_spawnp(
      &pid, exe_path.c_str(), nullptr, nullptr, argv.data(), environ);

  unsetenv("LYRA_FS_BASE_DIR");

  if (spawn_result != 0) {
    output.PrintError(
        std::format(
            "failed to execute '{}': {}", exe_path.string(),
            std::strerror(spawn_result)));
    output.Flush();
    fs::remove_all(temp_dir);
    return 1;
  }

  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    output.PrintError(std::format("waitpid failed: {}", std::strerror(errno)));
    output.Flush();
    fs::remove_all(temp_dir);
    return 1;
  }

  fs::remove_all(temp_dir);

  output.Flush();
  if (WIFEXITED(status)) {
    return WEXITSTATUS(status);
  }
  return 1;
}

}  // namespace lyra::driver
