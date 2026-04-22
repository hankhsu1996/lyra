#pragma once

#include <string>

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"

namespace lyra::test {

// Result of evaluating a CaseExecutionResult against TestCase expectations.
struct ExpectationVerdict {
  bool passed = false;
  std::string failure_message;
};

// Validate that a TestCase's expectations are supported for the given backend.
// Returns empty string if valid, or an error message describing the
// unsupported contract. Call before execution to reject invalid combinations
// rather than producing misleading results.
auto ValidateTestContract(const TestCase& test_case, BackendKind backend)
    -> std::string;

// Evaluate whether a CaseExecutionResult satisfies the expectations declared
// in a TestCase. Handles expected_runtime_fatal, expected_error, and normal
// success expectations (variables, time, stdout, compiler output, cover hits).
// Pure function -- no GTest, no side effects.
auto EvaluateExpectations(
    const TestCase& test_case, const CaseExecutionResult& result)
    -> ExpectationVerdict;

}  // namespace lyra::test
