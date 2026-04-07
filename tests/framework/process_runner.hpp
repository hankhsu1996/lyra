#pragma once

// Canonical process-execution primitives.
// All child-process spawning, capture, timeout, and termination decoding
// lives here. No other module should use fork/waitpid/posix_spawnp directly.

#include <chrono>
#include <filesystem>
#include <functional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "tests/framework/test_result.hpp"

namespace lyra::test {

// How a child process terminated.
enum class TerminationKind {
  kExitedNormally,  // exit(0)
  kExitedNonZero,   // exit(N) where N != 0
  kSignaled,        // killed by signal (e.g., SIGSEGV, SIGABRT)
  kTimedOut,        // killed after deadline exceeded
  kSpawnFailed,     // posix_spawnp or fork failed to create child
  kWaitFailed,      // waitpid failed after child was created
};

// Structured result from running a child process.
struct ProcessOutcome {
  TerminationKind termination = TerminationKind::kSpawnFailed;
  int exit_code = 0;
  int signal_number = 0;
  std::string stdout_text;
  std::string stderr_text;
};

// Environment variable override for subprocess execution.
using EnvOverrides = std::vector<std::pair<std::string, std::string>>;

// Spawn a child process, capture stdout and stderr concurrently, and
// enforce an optional timeout. If timeout is zero, no deadline is applied.
// On timeout the child is killed with SIGKILL and termination is kTimedOut.
auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    const EnvOverrides& env_overrides = {},
    std::chrono::seconds timeout = std::chrono::seconds{0}) -> ProcessOutcome;

// Fork the current process and run `action` in the child. Captures child
// stderr via pipe. Returns structured termination info.
//
// Temporary: only needed for the GTest parity harness (expected_runtime_fatal
// JIT tests that must crash in a subprocess to avoid killing the test runner).
// Remove when the old cc_test suite targets are deleted.
auto RunInFork(std::function<void()> action) -> ProcessOutcome;

// Map a ProcessOutcome from subprocess-backed execution into an
// ExecutionResult. `label` identifies the subprocess for error messages
// (e.g., "AOT executable", "LLI").
auto MapProcessOutcomeToExecutionResult(
    std::string_view label, const ProcessOutcome& proc) -> ExecutionResult;

// Format a tool-invocation failure for diagnostics. Distinguishes exit code,
// signal, timeout, and spawn failures instead of collapsing them all into
// "exit code N". Returns empty string for kExitedNormally.
auto FormatToolFailure(
    std::string_view tool_name, const std::filesystem::path& input,
    const std::filesystem::path& output, const ProcessOutcome& proc)
    -> std::string;

}  // namespace lyra::test
