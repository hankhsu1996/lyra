#pragma once

#include <filesystem>

#include "tests/framework/jit_backend.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Run test using LLI subprocess backend.
// work_directory is the runner-owned directory for file I/O (may be empty).
auto RunLliBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult;

}  // namespace lyra::test
