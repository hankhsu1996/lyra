#pragma once

#include <cstddef>
#include <expected>
#include <filesystem>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::support {

struct ProcessResult {
  int exit_code = 0;
  std::string stdout_text;
  std::string stderr_text;
};

// One process to run, held as a value so a batch of them can be described
// before any of it starts.
struct ProcessRequest {
  std::filesystem::path exe;
  std::vector<std::string> args;
};

// Resolve an executable `name` to an absolute path. An absolute or
// parent-qualified name is checked directly; a bare name is searched on PATH.
auto FindOnPath(std::string_view name)
    -> std::expected<std::filesystem::path, std::string>;

// Run `exe args...` capturing stdout and stderr. A non-zero exit code is a
// successful result (reported via ProcessResult::exit_code); an error is
// returned only when the process cannot be spawned or reaped.
auto RunProcessCaptured(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<ProcessResult, std::string>;

// Run every request, keeping at most `max_concurrent` of them alive at once,
// and answer with one result per request in the order given. Every request is
// run even where an earlier one exited non-zero, so a caller reports each
// failure rather than the first. An error is returned only when a process
// cannot be spawned or reaped.
auto RunProcessesCaptured(
    std::span<const ProcessRequest> requests, std::size_t max_concurrent)
    -> std::expected<std::vector<ProcessResult>, std::string>;

// Run `exe args...` inheriting this process's stdout and stderr, so the child's
// output streams straight to the terminal. Returns the child's exit code.
auto RunProcessStreaming(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<int, std::string>;

}  // namespace lyra::support
