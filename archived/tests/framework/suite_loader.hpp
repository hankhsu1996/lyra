#pragma once

#include <filesystem>
#include <string>
#include <vector>

#include "tests/framework/suite.hpp"

namespace lyra::test {

// Load a suite by name from the suites.yaml configuration file.
// The suites_path should point to the suites.yaml file.
// Throws std::runtime_error if suite not found or parsing fails.
auto LoadSuite(
    const std::filesystem::path& suites_path, const std::string& suite_name)
    -> Suite;

// Get list of YAML test files matching a suite's include/exclude patterns.
// The yaml_dir is the root directory containing test YAML files.
// Returns paths relative to yaml_dir (e.g., "operators/binary.yaml").
auto GetSuiteTestFiles(
    const Suite& suite, const std::filesystem::path& yaml_dir)
    -> std::vector<std::filesystem::path>;

}  // namespace lyra::test
