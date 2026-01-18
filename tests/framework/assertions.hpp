#ifndef TESTS_FRAMEWORK_ASSERTIONS_HPP
#define TESTS_FRAMEWORK_ASSERTIONS_HPP

#include <cstdint>
#include <filesystem>
#include <map>
#include <string>
#include <variant>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Normalize line endings to Unix style (\n)
auto NormalizeNewlines(std::string input) -> std::string;

// Assert output matches expected (exact or contains/not_contains)
void AssertOutput(const std::string& actual, const ExpectedOutput& expected);

// Assert files in directory match expected content
void AssertFiles(
    const std::filesystem::path& work_directory,
    const std::map<std::string, ExpectedOutput>& expected_files);

// Extracted variable value - matches ExpectedValue but for actual runtime
// values
using ExtractedValue = std::variant<int64_t, double, HexValue>;

// Assert variable values match expected values
void AssertVariables(
    const std::map<std::string, ExtractedValue>& actual,
    const std::map<std::string, ExpectedValue>& expected,
    const std::string& test_name);

}  // namespace lyra::test

#endif  // TESTS_FRAMEWORK_ASSERTIONS_HPP
