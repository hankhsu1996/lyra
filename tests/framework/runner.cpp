#include "tests/framework/runner.hpp"

#include <algorithm>
#include <filesystem>
#include <format>
#include <gtest/gtest.h>
#include <optional>
#include <string>

#include "lyra/common/mutation_event.hpp"
#include "tests/framework/assertions.hpp"
#include "tests/framework/jit_backend.hpp"
#include "tests/framework/lli_backend.hpp"
#include "tests/framework/mir_backend.hpp"
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
  if (test_case.IsMultiFile() || !test_case.expected_files.empty()) {
    work_directory = MakeUniqueTempPath(test_case.name);
    std::filesystem::create_directories(work_directory);
    workdir_guard.emplace(work_directory);
  }

  switch (backend) {
    case BackendKind::kMir: {
      auto result = RunMirInterpreter(test_case, work_directory);

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
        GTEST_SKIP() << "Time assertions not yet supported by MIR interpreter";
      }

      // Check expected stdout
      if (test_case.expected_stdout.has_value()) {
        AssertOutput(result.captured_output, test_case.expected_stdout.value());
      }

      // Check expected files
      if (!test_case.expected_files.empty()) {
        AssertFiles(work_directory, test_case.expected_files);
      }

      // Check expected mutations
      if (test_case.expected_mutations.has_value()) {
        const auto& me = *test_case.expected_mutations;
        if (me.min_count.has_value()) {
          EXPECT_GE(result.mutation_events.size(), *me.min_count)
              << "[" << test_case.source_yaml << "] Expected at least "
              << *me.min_count << " mutation events, got "
              << result.mutation_events.size();
        }
        for (const auto& kind_str : me.contains_kind) {
          bool found = false;
          if (kind_str == "value_write") {
            found = std::ranges::any_of(
                result.mutation_events, [](const common::MutationEvent& e) {
                  return e.kind == common::MutationKind::kValueWrite;
                });
          } else if (kind_str == "bulk_init") {
            found = std::ranges::any_of(
                result.mutation_events, [](const common::MutationEvent& e) {
                  return e.kind == common::MutationKind::kBulkInit;
                });
          } else if (kind_str == "structural_realloc") {
            found = std::ranges::any_of(
                result.mutation_events, [](const common::MutationEvent& e) {
                  return e.kind == common::MutationKind::kStructuralRealloc;
                });
          } else if (kind_str == "structural_no_realloc") {
            found = std::ranges::any_of(
                result.mutation_events, [](const common::MutationEvent& e) {
                  return e.kind == common::MutationKind::kStructuralNoRealloc;
                });
          } else if (kind_str == "structural") {
            // Backward compat: matches either structural kind.
            found = std::ranges::any_of(
                result.mutation_events, [](const common::MutationEvent& e) {
                  return e.kind == common::MutationKind::kStructuralNoRealloc ||
                         e.kind == common::MutationKind::kStructuralRealloc;
                });
          } else {
            FAIL() << "[" << test_case.source_yaml
                   << "] Unknown mutation kind: " << kind_str;
          }
          EXPECT_TRUE(found)
              << "[" << test_case.source_yaml << "] Expected mutation kind '"
              << kind_str << "' not found";
        }
      }
      break;
    }

    case BackendKind::kJit: {
      if (!test_case.expected_files.empty()) {
        GTEST_SKIP() << "JIT backend does not support file assertions";
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
      break;
    }

    case BackendKind::kLli: {
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
      break;
    }
  }
}

}  // namespace lyra::test
