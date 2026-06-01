#pragma once

#include <expected>
#include <filesystem>
#include <span>
#include <string>
#include <string_view>

namespace lyra::support {

struct ProcessResult {
  int exit_code = 0;
  std::string stdout_text;
  std::string stderr_text;
};

// Resolve an executable `name` to an absolute path. An absolute or
// parent-qualified name is checked directly; a bare name is searched on PATH.
auto FindOnPath(std::string_view name)
    -> std::expected<std::filesystem::path, std::string>;

// Resolve the C++ compiler to use for building emitted artifacts: `$CXX` when
// set, otherwise the first of clang++, g++, c++ found on PATH. The emitted code
// uses only standard C++23 features, so any of these compiles it.
auto ResolveCxxCompiler() -> std::expected<std::filesystem::path, std::string>;

// Run `exe args...` capturing stdout and stderr. A non-zero exit code is a
// successful result (reported via ProcessResult::exit_code); an error is
// returned only when the process cannot be spawned or reaped.
auto RunProcessCaptured(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<ProcessResult, std::string>;

// Run `exe args...` inheriting this process's stdout and stderr, so the child's
// output streams straight to the terminal. Returns the child's exit code.
auto RunProcessStreaming(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> std::expected<int, std::string>;

// Create a unique temporary directory and return its absolute path.
auto MakeTempDir() -> std::expected<std::filesystem::path, std::string>;

}  // namespace lyra::support
