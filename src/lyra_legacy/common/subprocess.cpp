#include "lyra/common/subprocess.hpp"

#include <array>
#include <cerrno>
#include <cstring>
#include <filesystem>
#include <optional>
#include <spawn.h>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <utility>
#include <vector>

namespace lyra::common {

auto RunSubprocess(
    const std::vector<std::string>& argv,
    const std::optional<std::filesystem::path>& working_dir)
    -> std::pair<int, std::string> {
  if (argv.empty()) {
    return {-1, "Empty argv"};
  }

  // Create pipe for stdout/stderr
  std::array<int, 2> pipe_fds{};
  if (pipe(pipe_fds.data()) != 0) {
    return {-1, "pipe() failed: " + std::string(strerror(errno))};
  }

  // Build argv array (must be null-terminated)
  std::vector<char*> c_argv;
  c_argv.reserve(argv.size() + 1);
  for (const auto& arg : argv) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
    c_argv.push_back(const_cast<char*>(arg.c_str()));
  }
  c_argv.push_back(nullptr);

  pid_t pid = 0;

  if (working_dir.has_value()) {
    // Use fork+exec when working_dir is specified
    // (posix_spawn_file_actions_addchdir_np is a GNU extension)
    pid = fork();
    if (pid == -1) {
      close(pipe_fds[0]);
      close(pipe_fds[1]);
      return {-1, "fork() failed: " + std::string(strerror(errno))};
    }

    if (pid == 0) {
      // Child process
      // Redirect stdout/stderr to pipe
      dup2(pipe_fds[1], STDOUT_FILENO);
      dup2(pipe_fds[1], STDERR_FILENO);
      close(pipe_fds[0]);
      close(pipe_fds[1]);

      // Change to working directory
      if (chdir(working_dir->c_str()) != 0) {
        // Can't report error nicely from child, just exit
        _exit(127);
      }

      // Execute
      execvp(c_argv[0], c_argv.data());
      // If execvp returns, it failed
      _exit(127);
    }
  } else {
    // Use posix_spawn when no working_dir (more efficient)
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init(&actions);
    posix_spawn_file_actions_adddup2(&actions, pipe_fds[1], STDOUT_FILENO);
    posix_spawn_file_actions_adddup2(&actions, pipe_fds[1], STDERR_FILENO);
    posix_spawn_file_actions_addclose(&actions, pipe_fds[0]);
    posix_spawn_file_actions_addclose(&actions, pipe_fds[1]);

    int spawn_result = posix_spawnp(
        &pid, c_argv[0], &actions, nullptr, c_argv.data(), environ);

    posix_spawn_file_actions_destroy(&actions);

    if (spawn_result != 0) {
      close(pipe_fds[0]);
      close(pipe_fds[1]);
      return {
          -1, "posix_spawnp() failed: " + std::string(strerror(spawn_result))};
    }
  }

  // Close write end in parent
  close(pipe_fds[1]);

  // Read output from pipe
  std::string output;
  std::array<char, 4096> buffer{};
  ssize_t bytes_read = 0;
  while ((bytes_read = read(pipe_fds[0], buffer.data(), buffer.size())) > 0) {
    output.append(buffer.data(), static_cast<size_t>(bytes_read));
  }
  close(pipe_fds[0]);

  // Wait for child
  int status = 0;
  waitpid(pid, &status, 0);

  int exit_code = -1;
  if (WIFEXITED(status)) {
    exit_code = WEXITSTATUS(status);
  } else if (WIFSIGNALED(status)) {
    exit_code = 128 + WTERMSIG(status);
  }

  return {exit_code, output};
}

}  // namespace lyra::common
