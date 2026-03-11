#pragma once

#include <filesystem>

#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"

namespace lyra::test {

// Run test using JIT backend (in-process ORC JIT).
// work_directory is the runner-owned directory for file I/O (may be empty).
auto RunJitBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state = false) -> TestResult;

}  // namespace lyra::test
