#pragma once

#include <cstdint>
#include <filesystem>
#include <map>
#include <string>
#include <variant>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Extracted value type for variable assertions
using ExtractedValue = std::variant<int64_t, double, HexValue>;

// Result from running a test backend
struct TestResult {
  bool success = false;
  std::string error_message;
  std::string captured_output;
  std::map<std::string, ExtractedValue> variables;
  uint64_t final_time = 0;
};

// Run test using LLVM JIT backend.
// work_directory is the runner-owned directory for file I/O (may be empty).
auto RunLlvmBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult;

}  // namespace lyra::test
