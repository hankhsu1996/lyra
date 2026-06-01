#include "lyra/support/subprocess.hpp"

#include <array>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <poll.h>
#include <span>
#include <spawn.h>
#include <string>
#include <string_view>
#include <sys/wait.h>
#include <system_error>
#include <unistd.h>
#include <vector>

namespace lyra::support {

namespace {

auto IsExecutableFile(const std::filesystem::path& path) -> bool {
  std::error_code ec;
  if (!std::filesystem::is_regular_file(path, ec) || ec) {
    return false;
  }
  return access(path.c_str(), X_OK) == 0;
}

auto BuildArgv(const std::string& exe, std::span<const std::string> args)
    -> std::vector<std::string> {
  std::vector<std::string> argv;
  argv.reserve(args.size() + 1);
  argv.push_back(exe);
  argv.insert(argv.end(), args.begin(), args.end());
  return argv;
}

auto ToCharPointers(std::vector<std::string>& argv) -> std::vector<char*> {
  std::vector<char*> ptrs;
  ptrs.reserve(argv.size() + 1);
  for (auto& arg : argv) {
    ptrs.push_back(arg.data());
  }
  ptrs.push_back(nullptr);
  return ptrs;
}

}  // namespace

auto FindOnPath(std::string_view name)
    -> std::expected<std::filesystem::path, std::string> {
  const std::filesystem::path candidate(name);
  if (candidate.is_absolute() || candidate.has_parent_path()) {
    auto absolute = std::filesystem::absolute(candidate);
    if (IsExecutableFile(absolute)) {
      return absolute;
    }
    return std::unexpected(
        std::format("'{}' is not an executable file", absolute.string()));
  }
  const char* path_env = std::getenv("PATH");
  if (path_env == nullptr) {
    return std::unexpected("PATH is unset");
  }
  std::string_view path(path_env);
  while (!path.empty()) {
    const auto sep = path.find(':');
    const auto entry = path.substr(0, sep);
    if (!entry.empty()) {
      auto full = std::filesystem::path(entry) / candidate;
      if (IsExecutableFile(full)) {
        return full;
      }
    }
    if (sep == std::string_view::npos) {
      break;
    }
    path.remove_prefix(sep + 1);
  }
  return std::unexpected(
      std::format("'{}' not found on PATH", candidate.string()));
}

auto ResolveCxxCompiler() -> std::expected<std::filesystem::path, std::string> {
  const char* env = std::getenv("CXX");
  if (env != nullptr) {
    const std::string_view sv(env);
    if (!sv.empty()) {
      return FindOnPath(sv);
    }
  }
  for (const std::string_view candidate : {"clang++", "g++", "c++"}) {
    auto found = FindOnPath(candidate);
    if (found) {
      return *found;
    }
  }
  return std::unexpected(
      "no C++ compiler found on PATH (tried clang++, g++, c++; set $CXX to "
      "override)");
}

auto RunProcessCaptured(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<ProcessResult, std::string> {
  std::array<int, 2> out_pipe{};
  std::array<int, 2> err_pipe{};
  if (pipe(out_pipe.data()) != 0 || pipe(err_pipe.data()) != 0) {
    return std::unexpected(
        std::format("failed to create pipe: {}", std::strerror(errno)));
  }

  posix_spawn_file_actions_t actions{};
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions, out_pipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_adddup2(&actions, err_pipe[1], STDERR_FILENO);
  posix_spawn_file_actions_addclose(&actions, out_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, out_pipe[1]);
  posix_spawn_file_actions_addclose(&actions, err_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, err_pipe[1]);

  std::string exe_str = exe.string();
  auto argv = BuildArgv(exe_str, args);
  auto argv_ptrs = ToCharPointers(argv);

  pid_t pid = 0;
  const int spawn_result = posix_spawn(
      &pid, exe_str.c_str(), &actions, nullptr, argv_ptrs.data(), environ);
  posix_spawn_file_actions_destroy(&actions);
  close(out_pipe[1]);
  close(err_pipe[1]);

  if (spawn_result != 0) {
    close(out_pipe[0]);
    close(err_pipe[0]);
    return std::unexpected(
        std::format(
            "failed to spawn '{}': {}", exe_str, std::strerror(spawn_result)));
  }

  ProcessResult result;
  std::array<struct pollfd, 2> fds{};
  fds[0] = {.fd = out_pipe[0], .events = POLLIN, .revents = 0};
  fds[1] = {.fd = err_pipe[0], .events = POLLIN, .revents = 0};
  const int out_fd = out_pipe[0];
  int open_fds = 2;
  std::array<char, 4096> buffer{};

  while (open_fds > 0) {
    const int ready = poll(fds.data(), fds.size(), -1);
    if (ready < 0) {
      if (errno == EINTR) {
        continue;
      }
      break;
    }
    for (auto& fd : fds) {
      if (fd.fd >= 0 && (fd.revents & (POLLIN | POLLHUP)) != 0) {
        const ssize_t n = read(fd.fd, buffer.data(), buffer.size());
        if (n > 0) {
          auto& target =
              (fd.fd == out_fd) ? result.stdout_text : result.stderr_text;
          target.append(buffer.data(), static_cast<std::size_t>(n));
        } else {
          close(fd.fd);
          fd.fd = -1;
          --open_fds;
        }
      }
    }
  }

  int status = 0;
  while (waitpid(pid, &status, 0) < 0) {
    if (errno != EINTR) {
      return std::unexpected(
          std::format("waitpid failed: {}", std::strerror(errno)));
    }
  }
  result.exit_code =
      WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
  return result;
}

auto RunProcessStreaming(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<int, std::string> {
  std::string exe_str = exe.string();
  auto argv = BuildArgv(exe_str, args);
  auto argv_ptrs = ToCharPointers(argv);

  pid_t pid = 0;
  const int spawn_result = posix_spawn(
      &pid, exe_str.c_str(), nullptr, nullptr, argv_ptrs.data(), environ);
  if (spawn_result != 0) {
    return std::unexpected(
        std::format(
            "failed to spawn '{}': {}", exe_str, std::strerror(spawn_result)));
  }

  int status = 0;
  while (waitpid(pid, &status, 0) < 0) {
    if (errno != EINTR) {
      return std::unexpected(
          std::format("waitpid failed: {}", std::strerror(errno)));
    }
  }
  return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
}

auto MakeTempDir() -> std::expected<std::filesystem::path, std::string> {
  const auto base = std::filesystem::temp_directory_path() / "lyra-XXXXXX";
  std::string templ = base.string();
  if (mkdtemp(templ.data()) == nullptr) {
    return std::unexpected(
        std::format(
            "mkdtemp('{}') failed: {}", base.string(), std::strerror(errno)));
  }
  return std::filesystem::path(templ);
}

}  // namespace lyra::support
