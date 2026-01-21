#include "tests/framework/assertions.hpp"

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <ios>
#include <iterator>
#include <map>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>

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

void AssertVariables(
    const std::map<std::string, ExtractedValue>& actual,
    const std::map<std::string, ExpectedValue>& expected,
    const std::string& test_name) {
  for (const auto& [name, expected_val] : expected) {
    auto it = actual.find(name);
    ASSERT_NE(it, actual.end())
        << "[" << test_name << "] Missing variable: " << name;

    // Convert to hex string for comparison
    auto to_hex = [](const ExtractedValue& val) -> std::string {
      return std::visit(
          [](auto&& v) -> std::string {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, HexValue>) {
              return v.hex;
            } else if constexpr (std::is_same_v<T, int64_t>) {
              return std::format("{:x}", static_cast<uint64_t>(v));
            } else {
              return std::format("{}", v);
            }
          },
          val);
    };

    // For double comparisons, keep the original type-safe comparison
    if (std::holds_alternative<double>(expected_val)) {
      const auto* actual_ptr = std::get_if<double>(&it->second);
      ASSERT_NE(actual_ptr, nullptr)
          << "[" << test_name << "] Type mismatch for variable " << name
          << " (expected double)";
      EXPECT_EQ(*actual_ptr, std::get<double>(expected_val))
          << "[" << test_name << "] Variable " << name;
    } else {
      // Compare via hex strings. For negative expected values (signed
      // comparison), we need to handle bit-width differences: actual values
      // are masked to their bit_width, while expected values are full 64-bit.
      // Solution: when expected is negative, trim expected_hex to match
      // actual_hex length (negative values have all-1s in high bits, so
      // trimming preserves the value).
      std::string expected_hex = to_hex(expected_val);
      std::string actual_hex = to_hex(it->second);

      // If expected is negative int64_t and longer than actual, trim it
      if (std::holds_alternative<int64_t>(expected_val) &&
          std::get<int64_t>(expected_val) < 0 &&
          expected_hex.size() > actual_hex.size()) {
        expected_hex =
            expected_hex.substr(expected_hex.size() - actual_hex.size());
      }

      EXPECT_EQ(actual_hex, expected_hex)
          << "[" << test_name << "] Variable " << name;
    }
  }
}

}  // namespace lyra::test
