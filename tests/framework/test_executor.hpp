#pragma once

#include <chrono>

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"

namespace lyra::test {

// Execute one test case in the current process context.
// Dispatches to the appropriate backend, returns structured result.
// For subprocess-backed backends (AOT/LLI), timeout is passed through
// to the child process execution. JIT ignores the timeout parameter
// (the caller owns timeout via fork).
// Contains no GTest macros, no fork, no special-casing.
auto ExecuteTestCase(
    const TestCase& test_case, BackendKind backend,
    bool force_two_state = false,
    std::chrono::seconds timeout = std::chrono::seconds{0})
    -> CaseExecutionResult;

}  // namespace lyra::test
