#include "tests/framework/assertions.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <ios>
#include <iterator>
#include <map>
#include <ranges>
#include <string>
#include <string_view>
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

// Format IntegralValue as SV-style binary string for error messages
auto FormatIntegral(const IntegralValue& v) -> std::string {
  std::string result;
  result.reserve(v.width);
  for (uint32_t i = v.width; i > 0; --i) {
    uint32_t bit_idx = i - 1;
    size_t word_idx = bit_idx / 64;
    uint32_t bit_in_word = bit_idx % 64;

    bool val_bit = word_idx < v.value.size() &&
                   ((v.value[word_idx] >> bit_in_word) & 1) != 0;
    bool unk_bit = word_idx < v.unknown.size() &&
                   ((v.unknown[word_idx] >> bit_in_word) & 1) != 0;

    if (!unk_bit) {
      result += val_bit ? '1' : '0';
    } else {
      result += val_bit ? 'z' : 'x';
    }
  }
  return std::format("{}'b{}", v.width, result);
}

// Check if IntegralValue has any unknown bits
auto HasUnknownBits(const IntegralValue& v) -> bool {
  return std::ranges::any_of(
      v.unknown, [](uint64_t word) { return word != 0; });
}

// Compare two IntegralValues for equality (same width, same planes)
auto IntegralEqual(const IntegralValue& a, const IntegralValue& b) -> bool {
  if (a.width != b.width) {
    return false;
  }
  size_t num_words = (a.width + 63) / 64;
  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a_val = i < a.value.size() ? a.value[i] : 0;
    uint64_t b_val = i < b.value.size() ? b.value[i] : 0;
    uint64_t a_unk = i < a.unknown.size() ? a.unknown[i] : 0;
    uint64_t b_unk = i < b.unknown.size() ? b.unknown[i] : 0;
    if (a_val != b_val || a_unk != b_unk) {
      return false;
    }
  }
  return true;
}

// Convert uint64_t to IntegralValue for comparison
auto Uint64ToIntegral(uint64_t val, uint32_t width) -> IntegralValue {
  IntegralValue result;
  result.width = width;
  size_t num_words = (width + 63) / 64;
  result.value.resize(num_words, 0);
  result.unknown.resize(num_words, 0);

  if (!result.value.empty()) {
    result.value[0] = val;
  }

  // Mask top word to actual width
  if (width > 0 && width % 64 != 0) {
    uint64_t mask = (uint64_t{1} << (width % 64)) - 1;
    result.value.back() &= mask;
  }
  return result;
}

void AssertVariables(
    const std::map<std::string, TestValue>& actual,
    const std::map<std::string, ExpectedValue>& expected,
    const std::string& test_name) {
  for (const auto& [name, expected_val] : expected) {
    auto it = actual.find(name);
    ASSERT_NE(it, actual.end())
        << "[" << test_name << "] Missing variable: " << name;

    const TestValue& actual_val = it->second;

    // Double comparison
    if (std::holds_alternative<double>(expected_val)) {
      const auto* actual_ptr = std::get_if<double>(&actual_val);
      ASSERT_NE(actual_ptr, nullptr)
          << "[" << test_name << "] Type mismatch for variable " << name
          << " (expected double)";
      EXPECT_EQ(*actual_ptr, std::get<double>(expected_val))
          << "[" << test_name << "] Variable " << name;
      continue;
    }

    // Integral comparison - both backends extract as IntegralValue
    const auto* actual_fs = std::get_if<IntegralValue>(&actual_val);
    ASSERT_NE(actual_fs, nullptr)
        << "[" << test_name << "] Type mismatch for variable " << name
        << " (expected integral)";

    // Convert expected to IntegralValue for uniform comparison
    IntegralValue expected_fs;
    if (std::holds_alternative<IntegralValue>(expected_val)) {
      expected_fs = std::get<IntegralValue>(expected_val);
    } else {
      // uint64_t expected - convert using actual's width
      uint64_t expected_int = std::get<uint64_t>(expected_val);
      expected_fs = Uint64ToIntegral(expected_int, actual_fs->width);
    }

    // Check for unexpected X/Z bits
    if (!HasUnknownBits(expected_fs) && HasUnknownBits(*actual_fs)) {
      FAIL() << "[" << test_name << "] Variable " << name
             << " has X/Z bits but expected 2-state value\n"
             << "  Actual: " << FormatIntegral(*actual_fs);
    }

    // Width must match for IntegralValue expected
    if (std::holds_alternative<IntegralValue>(expected_val)) {
      ASSERT_EQ(actual_fs->width, expected_fs.width)
          << "[" << test_name << "] Variable " << name << " width mismatch";
    }

    EXPECT_TRUE(IntegralEqual(*actual_fs, expected_fs))
        << "[" << test_name << "] Variable " << name << "\n"
        << "  Expected: " << FormatIntegral(expected_fs) << "\n"
        << "  Actual:   " << FormatIntegral(*actual_fs);
  }
}

}  // namespace lyra::test
