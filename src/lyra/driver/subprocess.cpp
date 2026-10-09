#include "lyra/driver/subprocess.hpp"

#include <algorithm>
#include <array>
#include <cerrno>
#include <chrono>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <functional>
#include <optional>
#include <poll.h>
#include <span>
#include <spawn.h>
#include <string>
#include <string_view>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <system_error>
#include <unistd.h>
#include <utility>
#include <vector>

#include "lyra/driver/signals.hpp"
#include "lyra/status/status.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::driver {

namespace {

// How much of each stream is kept. A tool that fails can print without end, and
// what tells a person why it failed is at the start.
constexpr std::size_t kKeptPerStream = std::size_t{4} * 1024 * 1024;
constexpr std::string_view kRestNotShown = "\n... the rest is not shown ...\n";

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

// A child as the run's statistics name it: the command line it was started
// with, which says what it was asked to do.
auto CommandLine(const std::vector<std::string>& argv) -> std::string {
  std::string line;
  for (const std::string& word : argv) {
    if (!line.empty()) {
      line += ' ';
    }
    line += word;
  }
  return line;
}

// A spawned child, the two pipes carrying its output, and what the run's
// statistics need to account for it once it is reaped. It keeps at least one
// open stream until both reach end of file, which is when it is reaped -- so a
// pool holding any child always has a descriptor to wait on.
struct Running {
  pid_t pid = 0;
  int out_fd = -1;
  int err_fd = -1;
  std::size_t index = 0;
  std::string command;
  std::chrono::steady_clock::time_point started;
};

// Which child's which stream a poll entry belongs to, held beside the entries
// because poll takes a contiguous array of its own.
struct StreamOwner {
  std::size_t child = 0;
  bool is_stdout = false;
};

// Starts `exe` with `argv`, its streams arranged by `actions` where any are
// given. This process holds off the signals that ask it to end so that one
// thread can answer them, and a child started as it is would hold them off too,
// so the child is started receiving every signal. Answers the child, or what
// the system said in refusing to start one.
auto Spawn(
    const std::string& exe, std::vector<std::string>& argv,
    const posix_spawn_file_actions_t* actions)
    -> std::expected<pid_t, std::string> {
  auto argv_ptrs = ToCharPointers(argv);
  sigset_t none_held;
  ::sigemptyset(&none_held);
  posix_spawnattr_t attributes{};
  ::posix_spawnattr_init(&attributes);
  ::posix_spawnattr_setsigmask(&attributes, &none_held);
  ::posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSIGMASK);
  int refused = 0;
  const std::optional<pid_t> child = StartChild([&]() -> std::optional<pid_t> {
    pid_t pid = 0;
    refused = ::posix_spawn(
        &pid, exe.c_str(), actions, &attributes, argv_ptrs.data(), environ);
    return refused == 0 ? std::optional<pid_t>{pid} : std::nullopt;
  });
  ::posix_spawnattr_destroy(&attributes);
  if (!child) {
    return std::unexpected(
        std::format("failed to spawn '{}': {}", exe, std::strerror(refused)));
  }
  return *child;
}

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

  const std::string exe_str = exe.string();
  auto argv = BuildArgv(exe_str, args);

  const auto started = std::chrono::steady_clock::now();
  const auto pid = Spawn(exe_str, argv, &actions);
  posix_spawn_file_actions_destroy(&actions);
  close(out_pipe[1]);
  close(err_pipe[1]);

  if (!pid) {
    close(out_pipe[0]);
    close(err_pipe[0]);
    return std::unexpected(pid.error());
  }
  return Running{
      .pid = *pid,
      .out_fd = out_pipe[0],
      .err_fd = err_pipe[0],
      .index = index,
      .command = CommandLine(argv),
      .started = started};
}

// How a child ended, and the processor time the system charged it.
struct Reaped {
  int exit_code = 0;
  std::uint64_t cpu_us = 0;
};

auto Microseconds(const struct timeval& time) -> std::uint64_t {
  return (static_cast<std::uint64_t>(time.tv_sec) * 1'000'000) +
         static_cast<std::uint64_t>(time.tv_usec);
}

// Waits for the child to end. Its exit code is its own, or 128 plus the signal
// that ended it, as a shell reports one. Its peak memory is not read: the
// system starts a spawned child's figure at its parent's high-water mark, so
// under a compiler holding a design it says how large the compiler was.
auto Reap(pid_t pid) -> std::expected<Reaped, std::string> {
  int status = 0;
  struct rusage usage{};
  while (wait4(pid, &status, 0, &usage) < 0) {
    if (errno != EINTR) {
      return std::unexpected(
          std::format("wait4 failed: {}", std::strerror(errno)));
    }
  }
  ChildReaped(pid);
  return Reaped{
      .exit_code =
          WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status),
      .cpu_us = Microseconds(usage.ru_utime) + Microseconds(usage.ru_stime)};
}

// Runs every request, at most `max_concurrent` at once, telling `started` and
// `ended` a request's position as it starts and as it ends.
auto RunProcesses(
    std::span<const ProcessRequest> requests, std::size_t max_concurrent,
    const std::function<void(std::size_t)>& started,
    const std::function<void(std::size_t)>& ended)
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
      started(next);
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
        // The note takes the text past the bound, so it is written once and
        // whatever is read after it is dropped.
        if (text.size() < kKeptPerStream) {
          text.append(
              buffer.data(),
              std::min(
                  static_cast<std::size_t>(n), kKeptPerStream - text.size()));
          if (text.size() == kKeptPerStream) {
            text += kRestNotShown;
          }
        }
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
      auto reaped = Reap(child->pid);
      if (!reaped) {
        return std::unexpected(std::move(reaped.error()));
      }
      results[child->index].exit_code = reaped->exit_code;
      support::RecordChild(
          std::move(child->command),
          support::ChildUsage{
              .wall_us = static_cast<std::uint64_t>(
                  std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::steady_clock::now() - child->started)
                      .count()),
              .cpu_us = reaped->cpu_us});
      ended(child->index);
      child = running.erase(child);
    }
  }

  if (spawn_failure.has_value()) {
    return std::unexpected(std::move(*spawn_failure));
  }
  return results;
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
  const auto unsaid = [](std::size_t) {};
  return RunProcesses(requests, max_concurrent, unsaid, unsaid);
}

auto RunProcessesCaptured(
    std::span<const ProcessRequest> requests, std::size_t max_concurrent,
    std::span<const std::string> names)
    -> std::expected<std::vector<ProcessResult>, std::string> {
  status::Pieces(requests.size());
  std::vector<std::optional<status::Piece>> pieces(requests.size());
  return RunProcesses(
      requests, max_concurrent,
      [&](std::size_t i) { pieces[i].emplace(names[i]); },
      [&](std::size_t i) { pieces[i].reset(); });
}

auto RunProcessStreaming(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<int, std::string> {
  const std::string exe_str = exe.string();
  auto argv = BuildArgv(exe_str, args);
  const auto pid = Spawn(exe_str, argv, nullptr);
  if (!pid) {
    return std::unexpected(pid.error());
  }

  auto reaped = Reap(*pid);
  if (!reaped) {
    return std::unexpected(std::move(reaped.error()));
  }
  return reaped->exit_code;
}

}  // namespace lyra::driver
