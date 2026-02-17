#include "tests/framework/suite_loader.hpp"

#include <algorithm>
#include <filesystem>
#include <format>
#include <regex>
#include <stdexcept>
#include <string>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "tests/framework/suite.hpp"

namespace lyra::test {

namespace {

// Normalize path separators to forward slash for cross-platform matching
auto NormalizePath(std::string path) -> std::string {
  std::ranges::replace(path, '\\', '/');
  return path;
}

// Compile regex pattern with error context
auto CompileRegex(const std::string& pattern, const std::string& context)
    -> std::regex {
  try {
    return std::regex(pattern);
  } catch (const std::regex_error& e) {
    throw std::runtime_error(
        std::format("{}: invalid regex '{}': {}", context, pattern, e.what()));
  }
}

// Check if path matches any of the compiled regex patterns
auto MatchesAny(
    const std::string& path, const std::vector<std::regex>& patterns) -> bool {
  return std::ranges::any_of(patterns, [&](const std::regex& re) {
    return std::regex_search(path, re);
  });
}

}  // namespace

auto LoadSuite(
    const std::filesystem::path& suites_path, const std::string& suite_name)
    -> Suite {
  if (!std::filesystem::exists(suites_path)) {
    throw std::runtime_error(
        std::format("Suite config file not found: {}", suites_path.string()));
  }

  YAML::Node root;
  try {
    root = YAML::LoadFile(suites_path.string());
  } catch (const YAML::Exception& e) {
    throw std::runtime_error(
        std::format(
            "{}: YAML parse error: {}", suites_path.string(), e.what()));
  }

  if (!root["suites"]) {
    throw std::runtime_error(
        std::format("{}: missing 'suites' key", suites_path.string()));
  }

  auto suites = root["suites"];
  if (!suites[suite_name]) {
    throw std::runtime_error(
        std::format(
            "{}: suite '{}' not found", suites_path.string(), suite_name));
  }

  auto suite_node = suites[suite_name];
  auto context = std::format("{}: suites.{}", suites_path.string(), suite_name);

  Suite suite;
  suite.name = suite_name;

  try {
    if (!suite_node["backend"]) {
      throw std::runtime_error(std::format("{}.backend is required", context));
    }
    suite.backend = ParseBackendKind(suite_node["backend"].as<std::string>());

    if (suite_node["two_state"]) {
      suite.force_two_state = suite_node["two_state"].as<bool>();
    }

    if (suite_node["include_regex"]) {
      for (const auto& pattern : suite_node["include_regex"]) {
        auto pat = pattern.as<std::string>();
        suite.include_regex.push_back(CompileRegex(pat, context));
      }
    }

    if (suite_node["exclude_regex"]) {
      for (const auto& pattern : suite_node["exclude_regex"]) {
        auto pat = pattern.as<std::string>();
        suite.exclude_regex.push_back(CompileRegex(pat, context));
      }
    }
  } catch (const YAML::Exception& e) {
    throw std::runtime_error(std::format("{}: {}", context, e.what()));
  }

  return suite;
}

auto GetSuiteTestFiles(
    const Suite& suite, const std::filesystem::path& yaml_dir)
    -> std::vector<std::filesystem::path> {
  if (!std::filesystem::exists(yaml_dir)) {
    throw std::runtime_error(
        std::format("Test directory not found: {}", yaml_dir.string()));
  }

  std::vector<std::filesystem::path> result;

  // Use follow_directory_symlink to handle Bazel runfiles symlinks
  auto iter_options =
      std::filesystem::directory_options::follow_directory_symlink;

  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(yaml_dir, iter_options)) {
    if (!entry.is_regular_file() || entry.path().extension() != ".yaml") {
      continue;
    }

    // Normalize relative path for matching
    auto rel_path =
        NormalizePath(entry.path().lexically_relative(yaml_dir).string());

    // Include if: no include patterns (= all) OR matches any include pattern
    bool included = suite.include_regex.empty() ||
                    MatchesAny(rel_path, suite.include_regex);

    if (!included) {
      continue;
    }

    // Exclude takes precedence
    if (MatchesAny(rel_path, suite.exclude_regex)) {
      continue;
    }

    result.push_back(entry.path());
  }

  std::ranges::sort(result);
  return result;
}

}  // namespace lyra::test
