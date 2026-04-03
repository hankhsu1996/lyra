#include "tests/framework/runner.hpp"

#include <filesystem>
#include <gtest/gtest.h>
#include <optional>
#include <string>

#include "tests/framework/aot_backend.hpp"
#include "tests/framework/assertions.hpp"
#include "tests/framework/fatal_subprocess.hpp"
#include "tests/framework/jit_backend.hpp"
#include "tests/framework/lli_backend.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

void RunTestCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state) {
  // Runner owns the work directory lifetime. Created when the test needs
  // file I/O (multi-file sources or file content assertions).
  std::optional<ScopedTempDirectory> workdir_guard;
  std::filesystem::path work_directory;
  if (test_case.IsMultiFile() || !test_case.expected_files.empty() ||
      !test_case.dpi_sources.empty()) {
    work_directory = MakeUniqueTempPath(test_case.name);
    std::filesystem::create_directories(work_directory);
    workdir_guard.emplace(work_directory);
  }

  switch (backend) {
    case BackendKind::kJit: {
      if (!test_case.expected_files.empty()) {
        GTEST_SKIP() << "JIT backend does not support file assertions";
      }

      // Runtime-fatal tests run in a subprocess via the encapsulated helper.
      if (test_case.expected_runtime_fatal.has_value()) {
        auto fatal_result = RunExpectingFatal(
            [&] { RunJitBackend(test_case, work_directory, force_two_state); });
        ASSERT_EQ(fatal_result.run_status, FatalSubprocessRunStatus::kRan)
            << "[" << test_case.source_yaml
            << "] Fatal subprocess helper failed to launch child";
        ASSERT_EQ(fatal_result.termination, ChildTerminationKind::kSignaled)
            << "[" << test_case.source_yaml
            << "] Expected signal-terminated child (abort), got "
            << (fatal_result.termination ==
                        ChildTerminationKind::kExitedNormally
                    ? "normal exit"
                    : "non-zero exit")
            << " (code=" << fatal_result.exit_code << ")";
        for (const auto& expected :
             test_case.expected_runtime_fatal->stderr_contains) {
          EXPECT_NE(
              fatal_result.captured_stderr.find(expected), std::string::npos)
              << "[" << test_case.source_yaml
              << "] Expected stderr to contain: " << expected
              << "\nActual stderr: " << fatal_result.captured_stderr;
        }
        break;
      }

      auto result = RunJitBackend(test_case, work_directory, force_two_state);

      if (test_case.expected_error.has_value()) {
        ASSERT_FALSE(result.success)
            << "[" << test_case.source_yaml
            << "] Expected compilation error but succeeded";
        AssertOutput(result.error_message, *test_case.expected_error);
        break;
      }

      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected variables
      if (!test_case.expected_values.empty()) {
        AssertVariables(
            result.variables, test_case.expected_values, test_case.source_yaml);
      }

      // Check expected time
      if (test_case.expected_time.has_value()) {
        EXPECT_EQ(result.final_time, *test_case.expected_time)
            << "[" << test_case.source_yaml << "] Time mismatch";
      }

      // Check expected stdout
      if (test_case.expected_stdout.has_value()) {
        AssertOutput(result.captured_output, test_case.expected_stdout.value());
      }

      // Check expected compiler output
      if (test_case.expected_compiler_output.has_value()) {
        AssertOutput(
            result.compiler_output, test_case.expected_compiler_output.value());
      }
      break;
    }

    case BackendKind::kLli: {
      if (test_case.expected_runtime_fatal.has_value()) {
        GTEST_SKIP()
            << "expect.runtime_fatal is only supported on the JIT backend";
      }
      if (!test_case.expected_files.empty()) {
        GTEST_SKIP() << "LLI backend does not support file assertions";
      }

      auto result = RunLliBackend(test_case, work_directory);

      if (test_case.expected_error.has_value()) {
        ASSERT_FALSE(result.success)
            << "[" << test_case.source_yaml
            << "] Expected compilation error but succeeded";
        AssertOutput(result.error_message, *test_case.expected_error);
        break;
      }

      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected variables
      if (!test_case.expected_values.empty()) {
        AssertVariables(
            result.variables, test_case.expected_values, test_case.source_yaml);
      }

      // Check expected time
      if (test_case.expected_time.has_value()) {
        EXPECT_EQ(result.final_time, *test_case.expected_time)
            << "[" << test_case.source_yaml << "] Time mismatch";
      }

      // Check expected stdout
      if (test_case.expected_stdout.has_value()) {
        AssertOutput(result.captured_output, test_case.expected_stdout.value());
      }

      // Check expected compiler output
      if (test_case.expected_compiler_output.has_value()) {
        AssertOutput(
            result.compiler_output, test_case.expected_compiler_output.value());
      }
      break;
    }

    case BackendKind::kAot: {
      if (test_case.expected_runtime_fatal.has_value()) {
        GTEST_SKIP()
            << "expect.runtime_fatal is only supported on the JIT backend";
      }
      if (!test_case.expected_files.empty()) {
        GTEST_SKIP() << "AOT backend does not support file assertions";
      }

      auto result = RunAotBackend(test_case, work_directory);

      if (test_case.expected_error.has_value()) {
        ASSERT_FALSE(result.success)
            << "[" << test_case.source_yaml
            << "] Expected compilation error but succeeded";
        AssertOutput(result.error_message, *test_case.expected_error);
        break;
      }

      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected variables
      if (!test_case.expected_values.empty()) {
        AssertVariables(
            result.variables, test_case.expected_values, test_case.source_yaml);
      }

      // Check expected time
      if (test_case.expected_time.has_value()) {
        EXPECT_EQ(result.final_time, *test_case.expected_time)
            << "[" << test_case.source_yaml << "] Time mismatch";
      }

      // Check expected stdout
      if (test_case.expected_stdout.has_value()) {
        AssertOutput(result.captured_output, test_case.expected_stdout.value());
      }

      // Check expected compiler output
      if (test_case.expected_compiler_output.has_value()) {
        AssertOutput(
            result.compiler_output, test_case.expected_compiler_output.value());
      }
      break;
    }
  }
}

}  // namespace lyra::test
