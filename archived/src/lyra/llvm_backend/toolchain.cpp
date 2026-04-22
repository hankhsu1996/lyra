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
  };
}

auto LinkExecutable(const Toolchain& toolchain, const LinkRequest& request)
    -> std::expected<std::filesystem::path, LinkError> {
  auto output_dir = request.output_path.parent_path();
  if (!output_dir.empty()) {
    std::error_code ec;
    fs::create_directories(output_dir, ec);
    if (ec) {
      return std::unexpected(
          LinkError{
              .stage = "link",
              .message = std::format(
                  "cannot create '{}': {}", output_dir.string(), ec.message()),
              .stderr = {},
          });
    }
  }

  // Link argument ordering is intentional: objects first (define symbols),
  // then runtime archives (pull referenced symbols), then external inputs
  // (DPI link inputs etc.), then system libs last. This order matters for
  // static archives where the linker resolves symbols left to right.
  std::vector<std::string> link_args = {
      toolchain.cc_path.string(),
      "-o",
      request.output_path.string(),
  };

  for (const auto& obj : request.object_inputs) {
    link_args.push_back(obj.string());
  }
  for (const auto& rt : request.runtime_link_inputs) {
    std::visit(
        [&link_args](const auto& input) {
          using T = std::decay_t<decltype(input)>;
          if constexpr (std::is_same_v<T, RuntimePathLinkInput>) {
            link_args.push_back(input.path.string());
          } else if constexpr (std::is_same_v<T, RuntimeSearchLinkInput>) {
            link_args.push_back(std::format("-L{}", input.search_dir.string()));
            link_args.push_back(std::format("-l:{}", input.library_name));
          }
        },
        rt);
  }
  for (const auto& ext : request.external_link_inputs) {
    link_args.push_back(ext.string());
  }
  for (const auto& sys : request.system_libs) {
    link_args.push_back(sys);
  }

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

  return request.output_path;
}

}  // namespace lyra::lowering::mir_to_llvm
