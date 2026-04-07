// Warm-parent test orchestrator.
//
// Loads manifest once, then runs each case via RunCase() which selects
// the isolation policy by backend:
//   JIT: fork per case (crash isolation + whole-case timeout)
//   AOT/LLI: direct execution (subprocess timeout only, no fork overhead)
// Evaluates expectations and prints per-case progress.

#include <chrono>
#include <cstdlib>
#include <exception>
#include <format>
#include <fstream>
#include <iostream>
#include <span>
#include <string>

#include "tests/framework/arg_parse.hpp"
#include "tests/framework/case_runner.hpp"
#include "tests/framework/expectation_eval.hpp"
#include "tests/framework/test_discovery.hpp"
#include "tests/framework/test_result.hpp"

namespace {

constexpr int kDefaultTimeoutSeconds = 30;

}  // namespace

auto main(int argc, char** argv) -> int {
  auto [args, remaining] =
      lyra::test::ParseArgs(std::span<char*>(argv, static_cast<size_t>(argc)));

  // Reject flags that do not belong to the supervisor.
  if (!args.test_file.empty()) {
    std::cerr << "supervisor: --test_file is not supported; use --suite\n";
    return 1;
  }
  if (!args.backend.empty()) {
    std::cerr << "supervisor: --backend is not supported; use --suite\n";
    return 1;
  }

  int timeout_seconds =
      args.timeout_seconds > 0 ? args.timeout_seconds : kDefaultTimeoutSeconds;

  auto shard = lyra::test::ResolveShardSpec(args);

  // Advertise sharding support to Bazel.
  const char* shard_status_file = std::getenv("TEST_SHARD_STATUS_FILE");
  if (shard_status_file != nullptr) {
    std::ofstream(shard_status_file).put('\0');
  }

  lyra::test::Manifest manifest;
  try {
    manifest = lyra::test::BuildManifest(args, shard);
  } catch (const std::exception& e) {
    std::cerr << std::format("supervisor: {}\n", e.what());
    return 1;
  }

  auto timeout = std::chrono::seconds{timeout_seconds};
  int passed = 0;
  int failed = 0;
  int crashed = 0;
  int timed_out = 0;
  int total = static_cast<int>(manifest.cases.size());

  std::cout << std::format("[==========] Running {} tests.\n", total);

  for (const auto& tc : manifest.cases) {
    // Skip cases with unsupported backend+expectation combinations.
    // These are valid tests that don't apply to this backend, not bugs.
    auto contract_error =
        lyra::test::ValidateTestContract(tc, manifest.backend);
    if (!contract_error.empty()) {
      continue;
    }

    auto t_start = std::chrono::steady_clock::now();

    auto result = lyra::test::RunCase(
        tc, manifest.backend, manifest.force_two_state, timeout);

    auto elapsed = std::chrono::duration<double>(
        std::chrono::steady_clock::now() - t_start);

    auto verdict = lyra::test::EvaluateExpectations(tc, result);

    if (verdict.passed) {
      ++passed;
      std::cout << std::format(
          "[       OK ] {} ({:.1f}s)\n", tc.name, elapsed.count());
    } else if (
        result.execution.outcome == lyra::test::ExecutionOutcome::kTimedOut) {
      ++timed_out;
      std::cout << std::format(
          "[ TIMEOUT  ] {} ({}s)\n", tc.name, timeout_seconds);
    } else if (
        result.execution.outcome == lyra::test::ExecutionOutcome::kCrashed &&
        !tc.expected_runtime_fatal.has_value()) {
      ++crashed;
      std::cout << std::format(
          "[  CRASH   ] {} (signal={})\n", tc.name,
          result.execution.signal_number);
    } else {
      ++failed;
      std::cout << std::format("[  FAILED  ] {}\n", tc.name);
      if (!verdict.failure_message.empty()) {
        std::cout << "  " << verdict.failure_message << "\n";
      }
    }
  }

  std::cout << std::format(
      "[==========] {} tests ran.\n"
      "[  PASSED  ] {} tests.\n",
      total, passed);
  if (failed > 0) {
    std::cout << std::format("[  FAILED  ] {} tests.\n", failed);
  }
  if (crashed > 0) {
    std::cout << std::format("[  CRASH   ] {} tests.\n", crashed);
  }
  if (timed_out > 0) {
    std::cout << std::format("[ TIMEOUT  ] {} tests.\n", timed_out);
  }

  return (failed + crashed + timed_out > 0) ? 1 : 0;
}
