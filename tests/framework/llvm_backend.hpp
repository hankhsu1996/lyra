#pragma once

#include <filesystem>
#include <map>
#include <string>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Result from running a test backend
struct TestResult {
  bool success = false;
  std::string error_message;
  std::string captured_output;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
};

// Parsed output from __LYRA_VAR and __LYRA_TIME__ protocol.
struct ParsedOutput {
  std::string clean;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
};

// Parse __LYRA_VAR and __LYRA_TIME__ output, stripping protocol lines.
// Shared between JIT and LLI backends.
auto ParseLyraVarOutput(const std::string& output) -> ParsedOutput;

// Run test using LLVM JIT backend.
// work_directory is the runner-owned directory for file I/O (may be empty).
auto RunLlvmBackend(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult;

}  // namespace lyra::test
