#include "tests/framework/test_discovery.hpp"

#include <algorithm>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <format>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "tests/framework/suite.hpp"
#include "tests/framework/suite_loader.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_case_loader.hpp"

namespace lyra::test {
namespace {

// Runfiles layout constants
constexpr std::string_view kYamlDirectory = "tests/sv_features";
constexpr std::string_view kSuitesFile = "tests/suites.yaml";

// Check if a path contains ".." components (path traversal attempt)
auto ContainsParentReference(const std::filesystem::path& path) -> bool {
  return std::ranges::any_of(path, [](const std::filesystem::path& component) {
    return component == "..";
  });
}

// Extract category from YAML path relative to yaml_directory.
// Uses full relative path with "/" replaced by "_" for uniqueness.
// e.g., "operators/binary.yaml" -> "operators_binary"
// e.g., "datatypes/arrays/packed.yaml" -> "datatypes_arrays_packed"
auto ExtractCategory(
    const std::filesystem::path& yaml_path,
    const std::filesystem::path& yaml_directory) -> std::string {
  auto relative = yaml_path.lexically_relative(yaml_directory);
  auto without_extension = relative.replace_extension("");
  std::string result = without_extension.string();
  std::ranges::replace(result, '/', '_');
  return result;
}

// Get the runfiles directory paths
struct RunfilesPaths {
  std::filesystem::path root;
  std::filesystem::path yaml_directory;
  std::filesystem::path suites_file;
};

auto GetRunfilesPaths() -> RunfilesPaths {
  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  const char* test_workspace = std::getenv("TEST_WORKSPACE");

  if (test_srcdir == nullptr || test_workspace == nullptr) {
    throw std::runtime_error("TEST_SRCDIR or TEST_WORKSPACE not set");
  }

  std::filesystem::path root =
      std::filesystem::path(test_srcdir) / test_workspace;

  return RunfilesPaths{
      .root = root,
      .yaml_directory = root / kYamlDirectory,
      .suites_file = root / kSuitesFile,
  };
}

}  // namespace

auto GetTestConfiguration(const CommandLineArgs& args) -> TestConfiguration {
  auto paths = GetRunfilesPaths();

  if (!args.test_file.empty()) {
    // Ad-hoc mode: single file
    if (args.backend.empty()) {
      throw std::runtime_error("--backend required with --test_file");
    }

    // Reject absolute paths early
    if (std::filesystem::path(args.test_file).is_absolute()) {
      throw std::runtime_error(
          "--test_file must be relative: " + args.test_file);
    }

    // Normalize and validate path
    auto base = paths.yaml_directory.lexically_normal();
    auto target = (base / args.test_file).lexically_normal();

    // Check for path traversal by examining components
    auto relative = target.lexically_relative(base);
    if (relative.empty() || ContainsParentReference(relative)) {
      throw std::runtime_error(
          std::format(
              "Invalid test file path (traversal attempt): {}",
              args.test_file));
    }

    // Use normalized path for existence check
    if (!std::filesystem::exists(target)) {
      throw std::runtime_error(
          std::format(
              "Test file not found: {} (resolved: {})", args.test_file,
              target.string()));
    }

    return TestConfiguration{
        .backend = ParseBackendKind(args.backend),
        .yaml_paths = {target},
        .yaml_directory = paths.yaml_directory,
    };
  }

  if (args.suite.empty()) {
    throw std::runtime_error("Must specify --suite or --test_file");
  }

  // Suite mode: load suite and get test files
  Suite suite;
  try {
    suite = LoadSuite(paths.suites_file, args.suite);
  } catch (const std::exception& e) {
    throw std::runtime_error(
        std::format(
            "Failed to load suite '{}' from {}: {}", args.suite,
            paths.suites_file.string(), e.what()));
  }

  auto yaml_paths = GetSuiteTestFiles(suite, paths.yaml_directory);
  if (yaml_paths.empty()) {
    throw std::runtime_error(
        std::format(
            "Suite '{}' matched no test files in {}", args.suite,
            paths.yaml_directory.string()));
  }

  return TestConfiguration{
      .backend = suite.backend,
      .yaml_paths = yaml_paths,
      .yaml_directory = paths.yaml_directory,
  };
}

auto LoadTestCases(
    const std::vector<std::filesystem::path>& yaml_paths,
    const std::filesystem::path& yaml_directory) -> std::vector<TestCase> {
  std::vector<TestCase> all_cases;

  for (const auto& yaml_path : yaml_paths) {
    auto cases = LoadTestCasesFromYaml(yaml_path.string());
    auto category = ExtractCategory(yaml_path, yaml_directory);
    auto yaml_relative = yaml_path.lexically_relative(yaml_directory).string();

    // Prefix test names with category and set source for error reporting
    for (auto& test_case : cases) {
      test_case.name = category + "_" + test_case.name;
      test_case.source_yaml = yaml_relative;
    }

    all_cases.insert(
        all_cases.end(), std::make_move_iterator(cases.begin()),
        std::make_move_iterator(cases.end()));
  }

  return all_cases;
}

}  // namespace lyra::test
