#pragma once

#include <cstdint>
#include <filesystem>
#include <map>
#include <string>

#include "tests/framework/test_case.hpp"
#include "tests/framework/test_value.hpp"

namespace lyra::test {

// Result from running a test backend
struct TestResult {
  bool success = false;
  std::string error_message;
  std::string captured_output;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
};

// Run test using JIT backend (in-process ORC JIT).
// work_directory is the runner-owned directory for file I/O (may be empty).
auto RunJitBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult;

}  // namespace lyra::test
