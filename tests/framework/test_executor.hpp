#pragma once

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"

namespace lyra::test {

// Execute one test case in the current process context.
// Dispatches to the appropriate backend, returns structured result.
// Contains no GTest macros, no fork, no special-casing.
auto ExecuteTestCase(
    const TestCase& test_case, BackendKind backend,
    bool force_two_state = false) -> CaseExecutionResult;

}  // namespace lyra::test
