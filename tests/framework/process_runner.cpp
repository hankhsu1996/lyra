#include "tests/framework/process_runner.hpp"

#include <algorithm>
#include <array>
#include <cerrno>
#include <chrono>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <fcntl.h>
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

// Decode waitpid status into TerminationKind + exit_code/signal_number.
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

// Non-blocking waitpid with EINTR retry. Returns pid on success (0 if child
// still running), or unexpected(errno) on real failure.
auto WaitPidNoHang(pid_t pid, int* status) -> std::expected<pid_t, int> {
  while (true) {
    pid_t w = waitpid(pid, status, WNOHANG);
    if (w >= 0) return w;
    if (errno == EINTR) continue;
    return std::unexpected(errno);
  }
}

// Blocking waitpid with EINTR retry. Returns success or unexpected(errno).
auto WaitPidBlocking(pid_t pid, int* status) -> std::expected<void, int> {
  while (true) {
    pid_t w = waitpid(pid, status, 0);
    if (w == pid) return {};
    if (w < 0 && errno == EINTR) continue;
    return std::unexpected(errno);
  }
}

// Compute poll timeout in ms from deadline. Returns -1 if no timeout.
auto ComputePollTimeoutMs(
    std::chrono::steady_clock::time_point deadline, bool has_timeout) -> int {
  if (!has_timeout) {
    return 100;  // Poll periodically to check child liveness
  }
  auto now = std::chrono::steady_clock::now();
  if (now >= deadline) {
    return 0;
  }
  auto remaining_ms = static_cast<int>(
      std::chrono::duration_cast<std::chrono::milliseconds>(deadline - now)
          .count());
  // Cap at 100ms so we check child liveness regularly
  return std::min(remaining_ms, 100);
}

// Build environment array with overrides applied to current environ.
struct OwnedEnviron {
  std::vector<std::string> entries;
  std::vector<char*> ptrs;
};

auto BuildEnvironWithOverrides(const EnvOverrides& overrides) -> OwnedEnviron {
  OwnedEnviron result;

  // Copy current environ
  // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  for (char** e = environ; *e != nullptr; ++e) {
    result.entries.emplace_back(*e);
  }
  // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)

  // Apply overrides
  for (const auto& [key, value] : overrides) {
    auto prefix = key + "=";
    bool replaced = false;
    for (auto& entry : result.entries) {
      if (entry.starts_with(prefix)) {
        entry = prefix + value;
        replaced = true;
        break;
      }
    }
    if (!replaced) {
      result.entries.push_back(prefix + value);
    }
  }

  result.ptrs.reserve(result.entries.size() + 1);
  for (auto& s : result.entries) {
    result.ptrs.push_back(s.data());
  }
  result.ptrs.push_back(nullptr);

  return result;
}

}  // namespace

auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    const EnvOverrides& env_overrides, std::chrono::seconds timeout)
    -> ProcessOutcome {
  // Create pipes for stdout and stderr
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

  // Build argv: [exe, args..., nullptr]
  std::string exe_str = exe.string();
  std::vector<char*> argv;
  argv.reserve(args.size() + 2);
  argv.push_back(exe_str.data());
  std::vector<std::string> arg_copies(args.begin(), args.end());
  for (auto& arg : arg_copies) {
    argv.push_back(arg.data());
  }
  argv.push_back(nullptr);

  // Build environment
  char** envp = environ;  // NOLINT(misc-include-cleaner)
  OwnedEnviron owned_env;
  if (!env_overrides.empty()) {
    owned_env = BuildEnvironWithOverrides(env_overrides);
    envp = owned_env.ptrs.data();
  }

  pid_t pid = 0;
  int spawn_result =
      posix_spawnp(&pid, exe_str.c_str(), &actions, nullptr, argv.data(), envp);
  posix_spawn_file_actions_destroy(&actions);

  // Close write ends in parent
  close(stdout_pipe[1]);
  close(stderr_pipe[1]);

  if (spawn_result != 0) {
    close(stdout_pipe[0]);
    close(stderr_pipe[0]);
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text = std::format(
            "posix_spawnp failed: {}", std::strerror(spawn_result))};
  }

  // Integrated event loop: drain pipes, check child liveness, enforce timeout.
  ProcessOutcome outcome;
  int status = 0;
  bool child_reaped = false;
  bool timed_out = false;
  bool has_timeout = (timeout != std::chrono::seconds{0});
  auto deadline = std::chrono::steady_clock::now() + timeout;

  std::array<struct pollfd, 2> fds{};
  fds[0] = {.fd = stdout_pipe[0], .events = POLLIN, .revents = 0};
  fds[1] = {.fd = stderr_pipe[0], .events = POLLIN, .revents = 0};
  int open_fds = 2;
  int stdout_fd = stdout_pipe[0];

  std::array<char, 4096> buffer{};

  while (open_fds > 0 || !child_reaped) {
    int poll_timeout_ms = ComputePollTimeoutMs(deadline, has_timeout);
    int ready = poll(fds.data(), fds.size(), poll_timeout_ms);
    if (ready < 0 && errno != EINTR) {
      // poll failed fatally -- kill child and reap
      kill(pid, SIGKILL);
      int discard = 0;
      (void)WaitPidBlocking(pid, &discard);
      outcome.termination = TerminationKind::kWaitFailed;
      outcome.stderr_text =
          std::format("poll failed: {}", std::strerror(errno));
      // Close any remaining fds
      for (auto& fd : fds) {
        if (fd.fd >= 0) {
          close(fd.fd);
          fd.fd = -1;
        }
      }
      return outcome;
    }

    // Drain readable pipe data
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

    // Check child liveness (non-blocking, EINTR-safe)
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

    // Check deadline
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

auto RunInFork(
    std::function<void(int result_fd)> action, std::chrono::seconds timeout)
    -> ProcessOutcome {
  // Create pipes: result pipe (child writes result data) and stderr pipe
  std::array<int, 2> result_pipe{};
  std::array<int, 2> stderr_pipe{};
  if (pipe(result_pipe.data()) != 0) {
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text = "failed to create result pipe"};
  }
  if (pipe(stderr_pipe.data()) != 0) {
    close(result_pipe[0]);
    close(result_pipe[1]);
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text = "failed to create stderr pipe"};
  }

  pid_t pid = fork();
  if (pid < 0) {
    close(result_pipe[0]);
    close(result_pipe[1]);
    close(stderr_pipe[0]);
    close(stderr_pipe[1]);
    return {
        .termination = TerminationKind::kSpawnFailed,
        .stdout_text = {},
        .stderr_text = "fork() failed"};
  }

  if (pid == 0) {
    // Child: redirect stderr to pipe, stdout to /dev/null, run action, exit.
    close(result_pipe[0]);
    close(stderr_pipe[0]);
    dup2(stderr_pipe[1], STDERR_FILENO);
    close(stderr_pipe[1]);
    // Redirect stdout to /dev/null so stray prints cannot corrupt the
    // parent's progress output. The structured result goes via result_fd.
    int devnull = open("/dev/null", O_WRONLY);  // NOLINT(misc-include-cleaner)
    if (devnull >= 0) {
      dup2(devnull, STDOUT_FILENO);
      close(devnull);
    }
    action(result_pipe[1]);
    close(result_pipe[1]);
    _exit(0);
  }

  // Parent: close write ends, run integrated event loop
  close(result_pipe[1]);
  close(stderr_pipe[1]);

  ProcessOutcome outcome;
  int status = 0;
  bool child_reaped = false;
  bool timed_out = false;
  bool has_timeout = (timeout != std::chrono::seconds{0});
  auto deadline = std::chrono::steady_clock::now() + timeout;

  std::array<struct pollfd, 2> fds{};
  fds[0] = {.fd = result_pipe[0], .events = POLLIN, .revents = 0};
  fds[1] = {.fd = stderr_pipe[0], .events = POLLIN, .revents = 0};
  int open_fds = 2;
  int result_fd = result_pipe[0];

  std::array<char, 4096> buffer{};

  while (open_fds > 0 || !child_reaped) {
    int poll_timeout_ms = ComputePollTimeoutMs(deadline, has_timeout);
    int ready = poll(fds.data(), fds.size(), poll_timeout_ms);
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

    // Drain readable pipe data
    for (auto& fd : fds) {
      if (fd.fd >= 0 && (fd.revents & (POLLIN | POLLHUP)) != 0) {
        ssize_t n = read(fd.fd, buffer.data(), buffer.size());
        if (n > 0) {
          auto& target =
              (fd.fd == result_fd) ? outcome.stdout_text : outcome.stderr_text;
          target.append(buffer.data(), static_cast<size_t>(n));
        } else {
          close(fd.fd);
          fd.fd = -1;
          --open_fds;
        }
      }
    }

    // Check child liveness (non-blocking, EINTR-safe)
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

    // Check deadline
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

auto MapProcessOutcomeToExecutionResult(
    std::string_view label, const ProcessOutcome& proc) -> ExecutionResult {
  ExecutionResult r;
  r.stderr_text = proc.stderr_text;
  switch (proc.termination) {
    case TerminationKind::kExitedNormally:
      r.outcome = ExecutionOutcome::kSuccess;
      return r;
    case TerminationKind::kExitedNonZero:
      r.outcome = ExecutionOutcome::kExecutionFailed;
      r.error_message =
          std::format("{} exited with code {}", label, proc.exit_code);
      r.exit_code = proc.exit_code;
      return r;
    case TerminationKind::kSignaled:
      r.outcome = ExecutionOutcome::kCrashed;
      r.error_message =
          std::format("{} killed by signal {}", label, proc.signal_number);
      r.signal_number = proc.signal_number;
      return r;
    case TerminationKind::kTimedOut:
      r.outcome = ExecutionOutcome::kTimedOut;
      r.error_message = std::format("{} timed out", label);
      return r;
    case TerminationKind::kSpawnFailed:
    case TerminationKind::kWaitFailed:
      r.outcome = ExecutionOutcome::kInfraError;
      r.error_message =
          std::format("{} infrastructure failure: {}", label, proc.stderr_text);
      return r;
  }
  return r;
}

auto FormatToolFailure(
    std::string_view tool_name, const std::filesystem::path& input,
    const std::filesystem::path& output, const ProcessOutcome& proc)
    -> std::string {
  switch (proc.termination) {
    case TerminationKind::kExitedNonZero:
      return std::format(
          "{} failed for '{}' -> '{}' (exit code {}): {}", tool_name,
          input.string(), output.string(), proc.exit_code, proc.stderr_text);
    case TerminationKind::kSignaled:
      return std::format(
          "{} killed by signal {} for '{}' -> '{}': {}", tool_name,
          proc.signal_number, input.string(), output.string(),
          proc.stderr_text);
    case TerminationKind::kTimedOut:
      return std::format(
          "{} timed out for '{}' -> '{}'", tool_name, input.string(),
          output.string());
    case TerminationKind::kSpawnFailed:
      return std::format(
          "{} failed to start for '{}' -> '{}': {}", tool_name, input.string(),
          output.string(), proc.stderr_text);
    case TerminationKind::kWaitFailed:
      return std::format(
          "{} wait failed for '{}' -> '{}': {}", tool_name, input.string(),
          output.string(), proc.stderr_text);
    case TerminationKind::kExitedNormally:
      return {};
  }
  return {};
}

}  // namespace lyra::test
