#pragma once

#include <filesystem>
#include <gtest/gtest.h>
#include <string>
#include <vector>

namespace lyra::test {

// Result of running a CLI command
struct CliResult {
  int exit_code;
  std::string stdout_output;
  std::string stderr_output;
  std::string combined_output;  // stdout + stderr interleaved

  [[nodiscard]] auto Success() const -> bool {
    return exit_code == 0;
  }
};

// Test fixture for CLI integration tests
//
// Provides utilities for:
// - Running the lyra binary with arguments
// - Managing temporary directories for test isolation
// - Creating test files (lyra.toml, .sv files)
//
// Usage:
//   TEST_F(CliTest, MyTest) {
//     auto result = Run({"init", "myproj"});
//     EXPECT_TRUE(result.Success());
//   }
//
class CliTestFixture : public ::testing::Test {
 protected:
  void SetUp() override;
  void TearDown() override;

  // Run lyra with given arguments from the test directory
  auto Run(std::initializer_list<std::string> args) -> CliResult;
  auto Run(const std::vector<std::string>& args) -> CliResult;

  // Run lyra from a specific directory
  auto RunIn(
      const std::filesystem::path& dir, std::initializer_list<std::string> args)
      -> CliResult;
  auto RunIn(
      const std::filesystem::path& dir, const std::vector<std::string>& args)
      -> CliResult;

  // Create a file in the test directory
  void WriteFile(
      const std::filesystem::path& relative_path, const std::string& content);

  // Create a minimal lyra.toml
  void WriteLyraToml(
      const std::string& name, const std::string& top,
      const std::vector<std::string>& files);

  // Create a simple SystemVerilog module
  void WriteSvModule(const std::string& filename, const std::string& name);

  // Get path to test directory
  [[nodiscard]] auto TestDir() const -> const std::filesystem::path& {
    return test_dir_;
  }

  // Check if a file exists in test directory
  [[nodiscard]] auto FileExists(
      const std::filesystem::path& relative_path) const -> bool;

  // Read file contents from test directory
  [[nodiscard]] auto ReadFile(const std::filesystem::path& relative_path) const
      -> std::string;

 private:
  std::filesystem::path test_dir_;
  std::filesystem::path lyra_bin_;

  auto RunImpl(
      const std::filesystem::path& working_dir,
      const std::vector<std::string>& args) -> CliResult;
};

}  // namespace lyra::test
