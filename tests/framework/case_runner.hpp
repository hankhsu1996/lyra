#pragma once

#include <chrono>

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"

namespace lyra::test {

// Run one test case using the correct isolation policy for the backend.
//
// JIT: fork per case. Timeout covers the entire case (frontend, lowering,
//   JIT compile, execution). Fork is required because in-process JIT
//   execution can crash the host.
// AOT/LLI: direct execution without fork. Timeout is applied only to the
//   simulation subprocess (the final compiled executable or lli process).
//   Frontend, lowering, object emission, and linking are not covered by
//   timeout. This is intentional: AOT/LLI simulation already runs in a
//   child process (inherent crash isolation), so forking the whole case
//   would add ~0.05s/case overhead with no safety benefit.
auto RunCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state,
    std::chrono::seconds timeout) -> CaseExecutionResult;

}  // namespace lyra::test
