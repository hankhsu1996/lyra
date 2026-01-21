#include "tests/framework/runner.hpp"

#include <gtest/gtest.h>

#include "tests/framework/assertions.hpp"
#include "tests/framework/llvm_backend.hpp"
#include "tests/framework/mir_backend.hpp"
#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

void RunTestCase(const TestCase& test_case, BackendKind backend) {
  switch (backend) {
    case BackendKind::kMir: {
      auto result = RunMirInterpreter(test_case);
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
        AssertFiles(result.work_directory, test_case.expected_files);
      }
      break;
    }

    case BackendKind::kLlvm: {
      if (test_case.expected_time.has_value()) {
        GTEST_SKIP() << "LLVM backend does not support time assertions";
      }
      if (!test_case.expected_files.empty()) {
        GTEST_SKIP() << "LLVM backend does not support file assertions";
      }

      auto result = RunLlvmBackend(test_case);
      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected variables
      if (!test_case.expected_values.empty()) {
        AssertVariables(
            result.variables, test_case.expected_values, test_case.source_yaml);
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
