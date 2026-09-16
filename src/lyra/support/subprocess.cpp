#include "lyra/support/subprocess.hpp"

#include <array>
#include <cerrno>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <poll.h>
#include <span>
#include <spawn.h>
#include <string>
#include <string_view>
#include <sys/wait.h>
#include <system_error>
#include <unistd.h>
#include <utility>
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

// A spawned child and the two pipes carrying its output. It keeps at least one
// open stream until both reach end of file, which is when it is reaped -- so a
// pool holding any child always has a descriptor to wait on.
struct Running {
  pid_t pid = 0;
  int out_fd = -1;
  int err_fd = -1;
  std::size_t index = 0;
};

// Which child's which stream a poll entry belongs to, held beside the entries
// because poll takes a contiguous array of its own.
struct StreamOwner {
  std::size_t child = 0;
  bool is_stdout = false;
};

auto SpawnCaptured(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::size_t index) -> std::expected<Running, std::string> {
  std::array<int, 2> out_pipe{};
  std::array<int, 2> err_pipe{};
  if (pipe(out_pipe.data()) != 0) {
    return std::unexpected(
        std::format("failed to create pipe: {}", std::strerror(errno)));
  }
  if (pipe(err_pipe.data()) != 0) {
    close(out_pipe[0]);
    close(out_pipe[1]);
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
  return Running{
      .pid = pid, .out_fd = out_pipe[0], .err_fd = err_pipe[0], .index = index};
}

auto Reap(pid_t pid) -> std::expected<int, std::string> {
  int status = 0;
  while (waitpid(pid, &status, 0) < 0) {
    if (errno != EINTR) {
      return std::unexpected(
          std::format("waitpid failed: {}", std::strerror(errno)));
    }
  }
  return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
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

auto RunProcessCaptured(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<ProcessResult, std::string> {
  const std::array<ProcessRequest, 1> one = {
      ProcessRequest{.exe = exe, .args = {args.begin(), args.end()}}};
  auto results = RunProcessesCaptured(one, 1);
  if (!results) {
    return std::unexpected(std::move(results.error()));
  }
  return std::move(results->front());
}

auto RunProcessesCaptured(
    std::span<const ProcessRequest> requests, std::size_t max_concurrent)
    -> std::expected<std::vector<ProcessResult>, std::string> {
  std::vector<ProcessResult> results(requests.size());
  std::vector<Running> running;
  std::optional<std::string> spawn_failure;
  std::size_t next = 0;

  while (next < requests.size() || !running.empty()) {
    // Never idle while work remains, which is also what makes a bound of none
    // one at a time rather than a stall.
    while (!spawn_failure.has_value() && next < requests.size() &&
           (running.empty() || running.size() < max_concurrent)) {
      auto spawned =
          SpawnCaptured(requests[next].exe, requests[next].args, next);
      if (!spawned) {
        spawn_failure = std::move(spawned.error());
        break;
      }
      running.push_back(*spawned);
      ++next;
    }
    if (running.empty()) {
      break;
    }

    std::vector<struct pollfd> fds;
    std::vector<StreamOwner> owners;
    for (std::size_t child = 0; child < running.size(); ++child) {
      for (const bool is_stdout : {true, false}) {
        const int fd =
            is_stdout ? running[child].out_fd : running[child].err_fd;
        if (fd < 0) {
          continue;
        }
        fds.push_back({.fd = fd, .events = POLLIN, .revents = 0});
        owners.push_back({.child = child, .is_stdout = is_stdout});
      }
    }

    const int ready = poll(fds.data(), fds.size(), -1);
    if (ready < 0) {
      if (errno == EINTR) {
        continue;
      }
      return std::unexpected(
          std::format("poll failed: {}", std::strerror(errno)));
    }

    std::array<char, 4096> buffer{};
    for (std::size_t entry = 0; entry < fds.size(); ++entry) {
      if ((fds[entry].revents & (POLLIN | POLLHUP | POLLERR)) == 0) {
        continue;
      }
      Running& child = running[owners[entry].child];
      ProcessResult& result = results[child.index];
      int& fd = owners[entry].is_stdout ? child.out_fd : child.err_fd;
      std::string& text =
          owners[entry].is_stdout ? result.stdout_text : result.stderr_text;
      const ssize_t n = read(fd, buffer.data(), buffer.size());
      if (n > 0) {
        text.append(buffer.data(), static_cast<std::size_t>(n));
        continue;
      }
      close(fd);
      fd = -1;
    }

    for (auto child = running.begin(); child != running.end();) {
      if (child->out_fd >= 0 || child->err_fd >= 0) {
        ++child;
        continue;
      }
      auto code = Reap(child->pid);
      if (!code) {
        return std::unexpected(std::move(code.error()));
      }
      results[child->index].exit_code = *code;
      child = running.erase(child);
    }
  }

  if (spawn_failure.has_value()) {
    return std::unexpected(std::move(*spawn_failure));
  }
  return results;
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

  return Reap(pid);
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
