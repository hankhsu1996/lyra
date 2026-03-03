#include "lyra/llvm_backend/toolchain.hpp"

#include <array>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <filesystem>
#include <format>
#include <spawn.h>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

namespace lyra::lowering::mir_to_llvm {

namespace fs = std::filesystem;

namespace {

// Run a command, capturing stderr. Returns exit code (-1 on spawn failure).
struct RunResult {
  int exit_code = -1;
  std::string captured_stderr;
};

auto RunCommand(const std::vector<std::string>& args) -> RunResult {
  // Set up stderr pipe
  std::array<int, 2> stderr_pipe{};
  if (pipe(stderr_pipe.data()) != 0) {
    return {.exit_code = -1, .captured_stderr = std::strerror(errno)};
  }

  posix_spawn_file_actions_t actions;
  posix_spawn_file_actions_init(&actions);
  // Redirect stdout to /dev/null (e.g., clang --version output)
  posix_spawn_file_actions_addopen(
      &actions, STDOUT_FILENO, "/dev/null", O_WRONLY, 0);
  posix_spawn_file_actions_adddup2(&actions, stderr_pipe[1], STDERR_FILENO);
  posix_spawn_file_actions_addclose(&actions, stderr_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, stderr_pipe[1]);

  // Build argv (posix_spawn needs char*[])
  std::vector<char*> argv;
  argv.reserve(args.size() + 1);
  for (const auto& arg : args) {
    argv.push_back(const_cast<char*>(arg.c_str()));
  }
  argv.push_back(nullptr);

  pid_t pid = 0;
  int spawn_result = posix_spawnp(
      &pid, args[0].c_str(), &actions, nullptr, argv.data(), environ);
  posix_spawn_file_actions_destroy(&actions);

  close(stderr_pipe[1]);

  if (spawn_result != 0) {
    close(stderr_pipe[0]);
    return {.exit_code = -1, .captured_stderr = std::strerror(spawn_result)};
  }

  // Read stderr from child
  std::string captured;
  std::array<char, 4096> buf{};
  ssize_t n = 0;
  while ((n = read(stderr_pipe[0], buf.data(), buf.size())) > 0) {
    captured.append(buf.data(), static_cast<size_t>(n));
  }
  close(stderr_pipe[0]);

  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    return {.exit_code = -1, .captured_stderr = std::strerror(errno)};
  }

  int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
  return {.exit_code = exit_code, .captured_stderr = std::move(captured)};
}

auto FindCC(bool allow_ambient_search) -> std::filesystem::path {
  // Check LYRA_CC environment variable first (explicit override)
  if (const char* lyra_cc = std::getenv("LYRA_CC")) {
    auto result = RunCommand({lyra_cc, "--version"});
    if (result.exit_code == 0) {
      return lyra_cc;
    }
  }
  // Check CC environment variable
  if (const char* cc_env = std::getenv("CC")) {
    auto result = RunCommand({cc_env, "--version"});
    if (result.exit_code == 0) {
      return cc_env;
    }
  }
  if (!allow_ambient_search) {
    return {};
  }
  // Try clang first (better C++23 support), then cc, then gcc
  for (const char* name : {"clang", "cc", "gcc"}) {
    auto result = RunCommand({name, "--version"});
    if (result.exit_code == 0) {
      return name;
    }
  }
  return {};
}

}  // namespace

auto DetectToolchain(bool allow_ambient_search)
    -> std::expected<Toolchain, std::string> {
  auto cc = FindCC(allow_ambient_search);
  if (cc.empty()) {
    return std::unexpected(
        "no C compiler found; set LYRA_CC or CC, or install clang/gcc");
  }
  return Toolchain{
      .cc_path = std::move(cc),
      .rpath_style = RpathStyle::kOrigin,
  };
}

auto LinkExecutable(
    const Toolchain& toolchain, const std::filesystem::path& object_path,
    const std::filesystem::path& runtime_lib_path,
    const std::filesystem::path& output_dir, const std::string& name)
    -> std::expected<std::filesystem::path, LinkError> {
  auto bin_dir = output_dir / "bin";
  auto lib_dir = output_dir / "lib";

  // Create output directories
  std::error_code ec;
  fs::create_directories(bin_dir, ec);
  if (ec) {
    return std::unexpected(
        LinkError{
            .stage = "link",
            .message = std::format(
                "cannot create '{}': {}", bin_dir.string(), ec.message()),
        });
  }
  fs::create_directories(lib_dir, ec);
  if (ec) {
    return std::unexpected(
        LinkError{
            .stage = "link",
            .message = std::format(
                "cannot create '{}': {}", lib_dir.string(), ec.message()),
        });
  }

  // Copy runtime library into bundle.
  // Remove existing file first: Bazel produces read-only files, so
  // copy_file(overwrite_existing) fails on the read-only destination.
  auto bundled_runtime = lib_dir / runtime_lib_path.filename();
  fs::remove(bundled_runtime, ec);
  // Ignore remove error (file may not exist)
  ec.clear();
  fs::copy_file(runtime_lib_path, bundled_runtime, ec);
  if (ec) {
    return std::unexpected(
        LinkError{
            .stage = "link",
            .message = std::format(
                "cannot copy runtime '{}' -> '{}': {}",
                runtime_lib_path.string(), bundled_runtime.string(),
                ec.message()),
        });
  }

  auto exe_path = bin_dir / name;

  // Build rpath flag
  std::string rpath_flag;
  switch (toolchain.rpath_style) {
    case RpathStyle::kOrigin:
      rpath_flag = "-Wl,-rpath,$ORIGIN/../lib";
      break;
    case RpathStyle::kLoaderPath:
      rpath_flag = "-Wl,-rpath,@loader_path/../lib";
      break;
  }

  // Link command
  std::vector<std::string> link_args = {
      toolchain.cc_path.string(),
      "-o",
      exe_path.string(),
      object_path.string(),
      "-L",
      lib_dir.string(),
      std::format("-l:{}", runtime_lib_path.filename().string()),
      rpath_flag,
      "-lm",
      "-lpthread",
  };

  auto result = RunCommand(link_args);
  if (result.exit_code != 0) {
    return std::unexpected(
        LinkError{
            .stage = "link",
            .message = std::format(
                "linker failed with exit code {}", result.exit_code),
            .stderr = std::move(result.captured_stderr),
        });
  }

  return exe_path;
}

}  // namespace lyra::lowering::mir_to_llvm
