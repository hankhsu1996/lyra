#pragma once

#include <chrono>
#include <filesystem>

#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"

namespace lyra::test {

// Run test using AOT backend (compile > link > subprocess execution).
// work_directory is the runner-owned directory for file I/O (may be empty).
// timeout is applied to the simulation subprocess execution.
auto RunAotBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    std::chrono::seconds timeout = std::chrono::seconds{0})
    -> CaseExecutionResult;

}  // namespace lyra::test
