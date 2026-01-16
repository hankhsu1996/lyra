#include "tests/framework/assertions.hpp"

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <ios>
#include <iterator>
#include <map>
#include <string>
#include <string_view>

#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

// Maximum characters to show in error messages to avoid CI log explosion
constexpr size_t kMaxOutputDisplay = 2048;

// Truncate string for display, showing first/last portions if too long
auto TruncateForDisplay(const std::string& str) -> std::string {
  if (str.size() <= kMaxOutputDisplay) {
    return str;
  }
  constexpr size_t kHalfSize = (kMaxOutputDisplay / 2) - 20;
  return str.substr(0, kHalfSize) + "\n... [" +
         std::to_string(str.size() - kMaxOutputDisplay) +
         " bytes truncated] ...\n" + str.substr(str.size() - kHalfSize);
}

// Common assertion logic for text matching (contains/not_contains)
void AssertTextMatches(
    const std::string& actual, const ExpectedOutput& expected,
    std::string_view context) {
  if (expected.IsExact()) {
    EXPECT_EQ(actual, expected.exact.value()) << context;
  } else {
    for (const auto& substring : expected.contains) {
      EXPECT_TRUE(actual.find(substring) != std::string::npos)
          << context << " should contain: \"" << substring << "\"\n"
          << "Actual (" << actual.size() << " bytes): \""
          << TruncateForDisplay(actual) << "\"";
    }
  }
  for (const auto& substring : expected.not_contains) {
    EXPECT_TRUE(actual.find(substring) == std::string::npos)
        << context << " should NOT contain: \"" << substring << "\"\n"
        << "Actual (" << actual.size() << " bytes): \""
        << TruncateForDisplay(actual) << "\"";
  }
}

// List files in directory for error messages
auto ListDirectoryFiles(const std::filesystem::path& directory) -> std::string {
  if (!std::filesystem::exists(directory)) {
    return "(directory does not exist)";
  }
  std::string files;
  for (const auto& entry : std::filesystem::directory_iterator(directory)) {
    if (!files.empty()) {
      files += ", ";
    }
    files += entry.path().filename().string();
  }
  return files.empty() ? "(empty)" : files;
}

}  // namespace

auto NormalizeNewlines(std::string input) -> std::string {
  std::erase(input, '\r');
  return input;
}

void AssertOutput(const std::string& actual, const ExpectedOutput& expected) {
  AssertTextMatches(actual, expected, "Output");
}

void AssertFiles(
    const std::filesystem::path& work_directory,
    const std::map<std::string, ExpectedOutput>& expected_files) {
  for (const auto& [filename, expected] : expected_files) {
    auto file_path = work_directory / filename;
    ASSERT_TRUE(std::filesystem::exists(file_path))
        << "Expected file not found: " << filename << "\n"
        << "Work directory: " << work_directory << "\n"
        << "Available files: " << ListDirectoryFiles(work_directory);

    std::ifstream input(file_path, std::ios::binary);
    ASSERT_TRUE(input.good()) << "Failed to open file: " << file_path;

    std::string actual(
        (std::istreambuf_iterator<char>(input)),
        std::istreambuf_iterator<char>());
    actual = NormalizeNewlines(actual);

    if (expected.IsExact()) {
      auto expected_normalized = NormalizeNewlines(expected.exact.value());
      EXPECT_EQ(actual, expected_normalized) << "File: " << filename;
    } else {
      AssertTextMatches(actual, expected, std::string("File ") + filename);
    }
  }
}

}  // namespace lyra::test
