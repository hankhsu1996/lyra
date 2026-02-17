#ifndef TESTS_FRAMEWORK_RUNNER_HPP
#define TESTS_FRAMEWORK_RUNNER_HPP

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Run a single test case with the specified backend
void RunTestCase(
    const TestCase& test_case, BackendKind backend,
    bool force_two_state = false);

}  // namespace lyra::test

#endif  // TESTS_FRAMEWORK_RUNNER_HPP
