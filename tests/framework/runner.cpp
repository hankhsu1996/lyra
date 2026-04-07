#include "tests/framework/runner.hpp"

#include <gtest/gtest.h>

#include "tests/framework/assertions.hpp"
#include "tests/framework/process_runner.hpp"
#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_executor.hpp"

namespace lyra::test {
namespace {

// Map a RunInFork ProcessOutcome to a CaseExecutionResult.
// Used for expected_runtime_fatal JIT tests in the GTest adapter.
auto MapForkOutcomeToCaseResult(const ProcessOutcome& proc)
    -> CaseExecutionResult {
  CaseExecutionResult result;
  result.execution =
      MapProcessOutcomeToExecutionResult("forked JIT execution", proc);
  return result;
}

// GTest-specific wrapper: fork-isolates expected-fatal JIT cases so the
// GTest runner survives the expected crash. The supervisor path does not
// need this -- process isolation is provided by the executor boundary.
auto ExecuteCaseForGTest(
    const TestCase& tc, BackendKind backend, bool force_two_state)
    -> CaseExecutionResult {
  if (backend == BackendKind::kJit && tc.expected_runtime_fatal.has_value()) {
    auto proc =
        RunInFork([&] { (void)ExecuteTestCase(tc, backend, force_two_state); });
    return MapForkOutcomeToCaseResult(proc);
  }
  return ExecuteTestCase(tc, backend, force_two_state);
}

}  // namespace

void RunTestCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state) {
  // Backend-specific skip checks (cannot be expressed in YAML expectations)
  if (!test_case.expected_files.empty()) {
    GTEST_SKIP() << "File assertions not yet supported";
  }
  if (test_case.expected_runtime_fatal.has_value() &&
      backend != BackendKind::kJit) {
    GTEST_SKIP() << "expect.runtime_fatal is only supported on the JIT backend";
  }

  auto result = ExecuteCaseForGTest(test_case, backend, force_two_state);
  AssertTestResult(test_case, result);
}

}  // namespace lyra::test
