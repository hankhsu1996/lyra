#include "process.hpp"

#include <algorithm>
#include <array>
#include <cerrno>
#include <chrono>
#include <csignal>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <poll.h>
#include <span>
#include <spawn.h>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

namespace lyra::test {
namespace {

auto DecodeWaitStatus(int status, ProcessOutcome& result) -> void {
  if (WIFSIGNALED(status)) {
    result.termination = TerminationKind::kSignaled;
    result.signal_number = WTERMSIG(status);
  } else if (WIFEXITED(status)) {
    result.exit_code = WEXITSTATUS(status);
    result.termination = (result.exit_code == 0)
                             ? TerminationKind::kExitedNormally
                             : TerminationKind::kExitedNonZero;
  }
}

auto WaitPidNoHang(pid_t pid, int* status) -> std::expected<pid_t, int> {
  while (true) {
    pid_t w = waitpid(pid, status, WNOHANG);
    if (w >= 0) {
      return w;
    }
    if (errno == EINTR) {
      continue;
    }
    return std::unexpected(errno);
  }
}

auto WaitPidBlocking(pid_t pid, int* status) -> std::expected<void, int> {
  while (true) {
    pid_t w = waitpid(pid, status, 0);
    if (w == pid) {
      return {};
    }
    if (w < 0 && errno == EINTR) {
      continue;
    }
    return std::unexpected(errno);
  }
}

auto ComputePollTimeoutMs(
    std::chrono::steady_clock::time_point deadline, bool has_timeout) -> int {
  if (!has_timeout) {
    return 100;
  }
  auto now = std::chrono::steady_clock::now();
  if (now >= deadline) {
    return 0;
  }
  auto remaining_ms = static_cast<int>(
      std::chrono::duration_cast<std::chrono::milliseconds>(deadline - now)
          .count());
  return std::min(remaining_ms, 100);
}

}  // namespace

auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::chrono::seconds timeout) -> ProcessOutcome {
  std::array<int, 2> stdout_pipe{};
  std::array<int, 2> stderr_pipe{};
  if (pipe(stdout_pipe.data()) != 0) {
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text = "failed to create stdout pipe"};
  }
  if (pipe(stderr_pipe.data()) != 0) {
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text = "failed to create stderr pipe"};
  }

  posix_spawn_file_actions_t actions{};
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_adddup2(&actions, stderr_pipe[1], STDERR_FILENO);
  posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, stdout_pipe[1]);
  posix_spawn_file_actions_addclose(&actions, stderr_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, stderr_pipe[1]);

  std::string exe_str = exe.string();
  std::vector<char*> argv;
  argv.reserve(args.size() + 2);
  argv.push_back(exe_str.data());
  std::vector<std::string> arg_copies(args.begin(), args.end());
  for (auto& arg : arg_copies) {
    argv.push_back(arg.data());
  }
  argv.push_back(nullptr);

  pid_t pid = 0;
  int spawn_result = posix_spawn(
      &pid, exe_str.c_str(), &actions, nullptr, argv.data(), environ);
  posix_spawn_file_actions_destroy(&actions);

  close(stdout_pipe[1]);
  close(stderr_pipe[1]);

  if (spawn_result != 0) {
    close(stdout_pipe[0]);
    close(stderr_pipe[0]);
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text =
            std::format("posix_spawn failed: {}", std::strerror(spawn_result))};
  }

  ProcessOutcome outcome;
  int status = 0;
  bool child_reaped = false;
  bool timed_out = false;
  const bool has_timeout = (timeout != std::chrono::seconds{0});
  auto deadline = std::chrono::steady_clock::now() + timeout;

  std::array<struct pollfd, 2> fds{};
  fds[0] = {.fd = stdout_pipe[0], .events = POLLIN, .revents = 0};
  fds[1] = {.fd = stderr_pipe[0], .events = POLLIN, .revents = 0};
  int open_fds = 2;
  const int stdout_fd = stdout_pipe[0];

  std::array<char, 4096> buffer{};

  while (open_fds > 0 || !child_reaped) {
    const int poll_timeout_ms = ComputePollTimeoutMs(deadline, has_timeout);
    const int ready = poll(fds.data(), fds.size(), poll_timeout_ms);
    if (ready < 0 && errno != EINTR) {
      kill(pid, SIGKILL);
      int discard = 0;
      (void)WaitPidBlocking(pid, &discard);
      outcome.termination = TerminationKind::kWaitFailed;
      outcome.stderr_text =
          std::format("poll failed: {}", std::strerror(errno));
      for (auto& fd : fds) {
        if (fd.fd >= 0) {
          close(fd.fd);
          fd.fd = -1;
        }
      }
      return outcome;
    }

    for (auto& fd : fds) {
      if (fd.fd >= 0 && (fd.revents & (POLLIN | POLLHUP)) != 0) {
        ssize_t n = read(fd.fd, buffer.data(), buffer.size());
        if (n > 0) {
          auto& target =
              (fd.fd == stdout_fd) ? outcome.stdout_text : outcome.stderr_text;
          target.append(buffer.data(), static_cast<size_t>(n));
        } else {
          close(fd.fd);
          fd.fd = -1;
          --open_fds;
        }
      }
    }

    if (!child_reaped) {
      auto wait_result = WaitPidNoHang(pid, &status);
      if (!wait_result) {
        outcome.termination = TerminationKind::kWaitFailed;
        outcome.stderr_text = std::format(
            "waitpid failed: {}", std::strerror(wait_result.error()));
        for (auto& fd : fds) {
          if (fd.fd >= 0) {
            close(fd.fd);
            fd.fd = -1;
          }
        }
        return outcome;
      }
      if (*wait_result == pid) {
        child_reaped = true;
      }
    }

    if (!child_reaped && has_timeout &&
        std::chrono::steady_clock::now() >= deadline) {
      kill(pid, SIGKILL);
      timed_out = true;
      auto reap_result = WaitPidBlocking(pid, &status);
      if (!reap_result) {
        outcome.termination = TerminationKind::kWaitFailed;
        outcome.stderr_text = std::format(
            "waitpid after SIGKILL failed: {}",
            std::strerror(reap_result.error()));
        for (auto& fd : fds) {
          if (fd.fd >= 0) {
            close(fd.fd);
            fd.fd = -1;
          }
        }
        return outcome;
      }
      child_reaped = true;
    }
  }

  if (timed_out) {
    outcome.termination = TerminationKind::kTimedOut;
    return outcome;
  }

  DecodeWaitStatus(status, outcome);
  return outcome;
}

}  // namespace lyra::test
