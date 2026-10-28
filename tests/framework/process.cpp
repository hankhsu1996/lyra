#include "process.hpp"

#include <algorithm>
#include <array>
#include <cerrno>
#include <charconv>
#include <chrono>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <fcntl.h>
#include <filesystem>
#include <format>
#include <gmock/gmock.h>
#include <optional>
#include <ostream>
#include <poll.h>
#include <span>
#include <spawn.h>
#include <string>
#include <string_view>
#include <sys/types.h>
#include <sys/wait.h>
#include <system_error>
#include <unistd.h>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/driver/subprocess.hpp"

namespace lyra::test {
namespace {

auto DecodeWaitStatus(int status) -> ProcessEnd {
  if (WIFSIGNALED(status)) {
    return Signalled{.signal = WTERMSIG(status)};
  }
  if (WIFEXITED(status)) {
    return Exited{.code = WEXITSTATUS(status)};
  }
  return CouldNotRun{
      .reason = std::format("waitpid reported status {:#x}", status)};
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

// Waits for a child that was just killed, so it does not linger as a zombie.
// A caller reaching here is already reporting a failure of its own, so neither
// the child's status nor a failure to collect it can change what it reports.
void ReapKilledChild(pid_t pid) {
  int status = 0;
  while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {
  }
}

// How long a child past its time limit has to end once asked, before it is
// killed. It is enough for a program to write a few lines, and it bounds the
// limit whatever the child does.
constexpr std::chrono::seconds kKillDelay{5};

// How long before the test's own end a child still running is out of time: the
// delay it is given once asked, and the time the test takes to report it.
constexpr std::chrono::seconds kReportMargin{15};

// When this process started, near enough: the runner counts a test's time from
// its start, and nothing this process does comes before its statics.
const std::chrono::steady_clock::time_point kStarted =
    std::chrono::steady_clock::now();

// When a child has to be out of the way for the test to report it, where the
// runner states the test's limit (Bazel's TEST_TIMEOUT, in seconds).
auto TestDeadline() -> std::optional<std::chrono::steady_clock::time_point> {
  const char* stated = std::getenv("TEST_TIMEOUT");
  if (stated == nullptr) {
    return std::nullopt;
  }
  const std::string_view text = stated;
  int seconds = 0;
  const auto [end, error] =
      std::from_chars(text.data(), text.data() + text.size(), seconds);
  if (error != std::errc{} || end != text.data() + text.size()) {
    return std::nullopt;
  }
  return kStarted + std::chrono::seconds{seconds} - kReportMargin;
}

auto Earlier(
    std::optional<std::chrono::steady_clock::time_point> a,
    std::optional<std::chrono::steady_clock::time_point> b)
    -> std::optional<std::chrono::steady_clock::time_point> {
  if (a && b) {
    return std::min(*a, *b);
  }
  return a ? a : b;
}

void CloseAll(std::span<struct pollfd> fds) {
  for (auto& fd : fds) {
    if (fd.fd >= 0) {
      close(fd.fd);
      fd.fd = -1;
    }
  }
}

auto ComputePollTimeoutMs(
    std::optional<std::chrono::steady_clock::time_point> deadline) -> int {
  if (!deadline) {
    return 100;
  }
  auto now = std::chrono::steady_clock::now();
  if (now >= *deadline) {
    return 0;
  }
  auto remaining_ms = static_cast<int>(
      std::chrono::duration_cast<std::chrono::milliseconds>(*deadline - now)
          .count());
  return std::min(remaining_ms, 100);
}

}  // namespace

auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::optional<std::chrono::seconds> timeout,
    const std::optional<std::filesystem::path>& working_directory)
    -> ProcessOutcome {
  // Every end is closed on exec, so a child started at the same moment from
  // another thread inherits none of them: a write end it held would keep this
  // reader waiting for its end of file until that other child exited. The
  // child keeps only the copies made onto its own output.
  std::array<int, 2> stdout_pipe{};
  std::array<int, 2> stderr_pipe{};
  if (pipe2(stdout_pipe.data(), O_CLOEXEC) != 0) {
    return {
        .end = CouldNotRun{.reason = "failed to create stdout pipe"},
        .stdout_text = {},
        .stderr_text = {}};
  }
  if (pipe2(stderr_pipe.data(), O_CLOEXEC) != 0) {
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    return {
        .end = CouldNotRun{.reason = "failed to create stderr pipe"},
        .stdout_text = {},
        .stderr_text = {}};
  }

  posix_spawn_file_actions_t actions{};
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_adddup2(&actions, stderr_pipe[1], STDERR_FILENO);
  if (working_directory) {
    posix_spawn_file_actions_addchdir_np(&actions, working_directory->c_str());
  }

  std::string exe_str = exe.string();
  std::vector<char*> argv;
  argv.reserve(args.size() + 2);
  argv.push_back(exe_str.data());
  std::vector<std::string> arg_copies(args.begin(), args.end());
  for (auto& arg : arg_copies) {
    argv.push_back(arg.data());
  }
  argv.push_back(nullptr);

  // The child leads a process group of its own, so that ending it ends what it
  // started too: a descendant left running holds the pipes open, and the wait
  // for them would last as long as the descendant does.
  posix_spawnattr_t attributes{};
  posix_spawnattr_init(&attributes);
  posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETPGROUP);
  posix_spawnattr_setpgroup(&attributes, 0);

  pid_t pid = 0;
  int spawn_result = posix_spawn(
      &pid, exe_str.c_str(), &actions, &attributes, argv.data(), environ);
  posix_spawnattr_destroy(&attributes);
  posix_spawn_file_actions_destroy(&actions);

  close(stdout_pipe[1]);
  close(stderr_pipe[1]);

  if (spawn_result != 0) {
    close(stdout_pipe[0]);
    close(stderr_pipe[0]);
    return {
        .end =
            CouldNotRun{
                .reason = std::format(
                    "posix_spawn failed: {}", std::strerror(spawn_result))},
        .stdout_text = {},
        .stderr_text = {}};
  }

  ProcessOutcome outcome;
  int status = 0;
  bool child_reaped = false;
  // The caller's limit and the test's own end are kept apart, because only
  // running past the first says anything about the child. A child past either
  // is asked to end, then killed if it has not once the delay is over, as
  // `timeout -k` and Bazel's own test runner do.
  std::optional<std::chrono::steady_clock::time_point> own_deadline;
  if (timeout) {
    own_deadline = std::chrono::steady_clock::now() + *timeout;
  }
  std::optional<std::chrono::steady_clock::time_point> deadline =
      Earlier(own_deadline, TestDeadline());
  std::optional<ProcessEnd> ended_for_time;

  std::array<struct pollfd, 2> fds{};
  fds[0] = {.fd = stdout_pipe[0], .events = POLLIN, .revents = 0};
  fds[1] = {.fd = stderr_pipe[0], .events = POLLIN, .revents = 0};
  int open_fds = 2;
  const int stdout_fd = stdout_pipe[0];

  std::array<char, 4096> buffer{};

  while (open_fds > 0 || !child_reaped) {
    const int poll_timeout_ms = ComputePollTimeoutMs(deadline);
    const int ready = poll(fds.data(), fds.size(), poll_timeout_ms);
    if (ready < 0 && errno != EINTR) {
      kill(-pid, SIGKILL);
      ReapKilledChild(pid);
      outcome.end = CouldNotRun{
          .reason = std::format("poll failed: {}", std::strerror(errno))};
      CloseAll(fds);
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
        outcome.end = CouldNotRun{
            .reason = std::format(
                "waitpid failed: {}", std::strerror(wait_result.error()))};
        CloseAll(fds);
        return outcome;
      }
      if (*wait_result == pid) {
        child_reaped = true;
      }
    }

    const auto now = std::chrono::steady_clock::now();
    if (deadline && now >= *deadline) {
      if (!ended_for_time) {
        kill(-pid, SIGTERM);
        ended_for_time = own_deadline && now >= *own_deadline
                             ? ProcessEnd{TimedOut{}}
                             : ProcessEnd{TestTimedOut{}};
        deadline = now + kKillDelay;
      } else {
        kill(-pid, SIGKILL);
        if (!child_reaped) {
          ReapKilledChild(pid);
          child_reaped = true;
        }
        deadline.reset();
      }
    }
  }

  outcome.end = ended_for_time ? *ended_for_time : DecodeWaitStatus(status);
  return outcome;
}

auto RunShellScript(
    std::string_view script, std::span<const std::string> positional,
    std::optional<std::chrono::seconds> timeout) -> ProcessOutcome {
  const auto sh = driver::FindOnPath("sh");
  if (!sh) {
    return {
        .end = CouldNotRun{.reason = std::string{sh.error().Text()}},
        .stdout_text = {},
        .stderr_text = {}};
  }
  std::vector<std::string> shell_args = {"-c", std::string{script}};
  shell_args.insert(shell_args.end(), positional.begin(), positional.end());
  return RunChildProcess(*sh, shell_args, timeout);
}

auto RunUnderAddressLimit(
    const std::filesystem::path& program, std::span<const std::string> args,
    int kibibytes) -> ProcessOutcome {
  std::vector<std::string> positional = {program.string()};
  positional.insert(positional.end(), args.begin(), args.end());
  return RunShellScript(
      std::format(R"(ulimit -v {} && exec "$0" "$@")", kibibytes), positional);
}

auto ExitCodeOf(const ProcessOutcome& outcome) -> std::optional<int> {
  if (const auto* exited = std::get_if<Exited>(&outcome.end)) {
    return exited->code;
  }
  return std::nullopt;
}

auto Describe(const ProcessEnd& end) -> std::string {
  return std::visit(
      Overloaded{
          [](const CouldNotRun& failed) {
            return std::format("could not be run: {}", failed.reason);
          },
          [](const Exited& exited) {
            return std::format("exited with {}", exited.code);
          },
          [](const Signalled& signalled) {
            return std::format("ended by signal {}", signalled.signal);
          },
          [](const TimedOut&) { return std::string{"ran out of its time"}; },
          [](const TestTimedOut&) {
            return std::string{"was ended because its test ran out of time"};
          },
      },
      end);
}

auto Transcript(
    const std::filesystem::path& exe, std::span<const std::string> args,
    const ProcessOutcome& outcome) -> std::string {
  std::string command = exe.string();
  for (const std::string& arg : args) {
    command += ' ';
    command += arg;
  }
  return std::format(
      "$ {}\n{}\nstdout:\n{}\nstderr:\n{}\n", command, Describe(outcome.end),
      outcome.stdout_text, outcome.stderr_text);
}

void PrintTo(const ProcessOutcome& outcome, std::ostream* out) {
  *out << "a child that " << Describe(outcome.end);
}

namespace {

class ExitedWithCodeMatcher {
 public:
  using is_gtest_matcher = void;

  explicit ExitedWithCodeMatcher(testing::Matcher<int> code)
      : code_(std::move(code)) {
  }

  auto MatchAndExplain(
      const ProcessOutcome& outcome,
      testing::MatchResultListener* listener) const -> bool {
    const std::optional<int> code = ExitCodeOf(outcome);
    return code && testing::ExplainMatchResult(code_, *code, listener);
  }

  void DescribeTo(std::ostream* out) const {
    *out << "exited with a code that ";
    code_.DescribeTo(out);
  }

  void DescribeNegationTo(std::ostream* out) const {
    *out << "did not exit with a code that ";
    code_.DescribeTo(out);
  }

 private:
  testing::Matcher<int> code_;
};

}  // namespace

auto ExitedWithCode(const testing::Matcher<int>& code)
    -> testing::Matcher<const ProcessOutcome&> {
  return ExitedWithCodeMatcher(code);
}

}  // namespace lyra::test
