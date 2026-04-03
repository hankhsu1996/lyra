#include "tests/framework/fatal_subprocess.hpp"

#include <array>
#include <sys/wait.h>
#include <unistd.h>

namespace lyra::test {

auto RunExpectingFatal(std::function<void()> action) -> FatalSubprocessResult {
  std::array<int, 2> pipe_fds{};
  if (pipe(pipe_fds.data()) != 0) {
    return {.run_status = FatalSubprocessRunStatus::kPipeFailed};
  }

  pid_t pid = fork();
  if (pid < 0) {
    close(pipe_fds[0]);
    close(pipe_fds[1]);
    return {.run_status = FatalSubprocessRunStatus::kForkFailed};
  }

  if (pid == 0) {
    // Child: redirect stderr to pipe write end, run action, exit.
    close(pipe_fds[0]);
    dup2(pipe_fds[1], STDERR_FILENO);
    close(pipe_fds[1]);
    action();
    _exit(0);
  }

  // Parent: read stderr from child, then wait.
  close(pipe_fds[1]);
  std::string captured;
  std::array<char, 1024> buf{};
  while (true) {
    auto n = read(pipe_fds[0], buf.data(), buf.size());
    if (n <= 0) break;
    captured.append(buf.data(), static_cast<size_t>(n));
  }
  close(pipe_fds[0]);

  int status = 0;
  if (waitpid(pid, &status, 0) < 0) {
    return {
        .run_status = FatalSubprocessRunStatus::kWaitFailed,
        .captured_stderr = std::move(captured),
    };
  }

  FatalSubprocessResult result{
      .run_status = FatalSubprocessRunStatus::kRan,
      .captured_stderr = std::move(captured),
  };

  if (WIFSIGNALED(status)) {
    result.termination = ChildTerminationKind::kSignaled;
    result.signal_number = WTERMSIG(status);
  } else if (WIFEXITED(status)) {
    result.exit_code = WEXITSTATUS(status);
    result.termination = (result.exit_code == 0)
                             ? ChildTerminationKind::kExitedNormally
                             : ChildTerminationKind::kExitedNonZero;
  }

  return result;
}

}  // namespace lyra::test
