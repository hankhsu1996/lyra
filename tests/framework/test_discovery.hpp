#ifndef TESTS_FRAMEWORK_TEST_DISCOVERY_HPP
#define TESTS_FRAMEWORK_TEST_DISCOVERY_HPP

#include <filesystem>
#include <string>
#include <vector>

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Parsed command-line arguments
struct CommandLineArgs {
  std::string suite;
  std::string backend;
  std::string test_file;
};

// Result of resolving args to test configuration
struct TestConfiguration {
  BackendKind backend;
  bool force_two_state = false;
  std::vector<std::filesystem::path> yaml_paths;
  std::filesystem::path yaml_directory;  // Base path for relative calculations
};

// Get test configuration based on parsed args
auto GetTestConfiguration(const CommandLineArgs& args) -> TestConfiguration;

// Load test cases from YAML files, prefixing names with category
auto LoadTestCases(
    const std::vector<std::filesystem::path>& yaml_paths,
    const std::filesystem::path& yaml_directory) -> std::vector<TestCase>;

}  // namespace lyra::test

#endif  // TESTS_FRAMEWORK_TEST_DISCOVERY_HPP
