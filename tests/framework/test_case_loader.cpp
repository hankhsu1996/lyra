#include "tests/framework/test_case_loader.hpp"

#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <format>
#include <initializer_list>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

namespace {

// Format YAML location for error messages
auto FormatLocation(const std::string& path, const YAML::Mark& mark)
    -> std::string {
  return std::format("{}:{}", path, mark.line + 1);
}

// Check if a key is in the allowed list
auto IsAllowedKey(
    std::string_view key, std::initializer_list<std::string_view> allowed)
    -> bool {
  return std::ranges::any_of(allowed, [key](std::string_view allowed_key) {
    return key == allowed_key;
  });
}

void ValidateKeys(
    const YAML::Node& node, std::initializer_list<std::string_view> allowed,
    std::string_view context, const std::string& file_path) {
  if (!node.IsMap()) {
    return;
  }
  for (const auto& pair : node) {
    auto key = pair.first.as<std::string>();
    if (!IsAllowedKey(key, allowed)) {
      throw std::runtime_error(
          std::format(
              "{}: Unknown field '{}' in {}",
              FormatLocation(file_path, pair.first.Mark()), key, context));
    }
  }
}

void ValidateIsSequence(
    const YAML::Node& node, std::string_view field_name,
    std::string_view context, const std::string& file_path) {
  if (node && !node.IsSequence()) {
    throw std::runtime_error(
        std::format(
            "{}: '{}' must be a list in {}",
            FormatLocation(file_path, node.Mark()), field_name, context));
  }
}

void ValidateIsScalar(
    const YAML::Node& node, std::string_view field_name,
    std::string_view context, const std::string& file_path) {
  if (node && !node.IsScalar()) {
    throw std::runtime_error(
        std::format(
            "{}: '{}' must be a string in {}",
            FormatLocation(file_path, node.Mark()), field_name, context));
  }
}

// Validate filename is a safe relative path within work directory
auto ValidateAndNormalizeFilename(
    const std::string& filename, const YAML::Mark& mark,
    const std::string& file_path, std::string_view context) -> std::string {
  std::filesystem::path path(filename);

  // Reject absolute paths
  if (path.is_absolute()) {
    throw std::runtime_error(
        std::format(
            "{}: Invalid filename '{}' in {} - must be relative path",
            FormatLocation(file_path, mark), filename, context));
  }

  // Normalize and check for ".." components
  auto normalized = path.lexically_normal();
  for (const auto& component : normalized) {
    if (component == "..") {
      throw std::runtime_error(
          std::format(
              "{}: Invalid filename '{}' in {} - path cannot escape work "
              "directory",
              FormatLocation(file_path, mark), filename, context));
    }
  }

  // Return canonical form for consistent map keys
  return normalized.generic_string();
}

// Parse numeric value: hex string for wide values, integer, or double
auto ParseNumericValue(const YAML::Node& node) -> ExpectedValue {
  auto str = node.as<std::string>();

  // Check for hex prefix (0x or 0X)
  if (str.starts_with("0x") || str.starts_with("0X")) {
    std::string hex = str.substr(2);
    std::ranges::transform(hex, hex.begin(), ::tolower);

    // Remove leading zeros for canonical form
    auto first_nonzero = hex.find_first_not_of('0');
    if (first_nonzero != std::string::npos) {
      hex = hex.substr(first_nonzero);
    } else {
      hex = "0";
    }

    // If fits in 64 bits (16 hex digits or less), parse as int64_t
    if (hex.size() <= 16) {
      return static_cast<int64_t>(std::stoull(hex, nullptr, 16));
    }
    // Wide value: store as HexValue
    return HexValue{hex};
  }

  bool has_decimal = str.find('.') != std::string::npos;
  bool has_exponent =
      str.find('e') != std::string::npos || str.find('E') != std::string::npos;

  if (has_decimal || has_exponent) {
    return node.as<double>();
  }
  return node.as<int64_t>();
}

// Parse ExpectedOutput from scalar (exact match) or map (contains/not_contains)
auto ParseExpectedOutput(
    const YAML::Node& node, std::string_view context,
    const std::string& file_path) -> ExpectedOutput {
  ExpectedOutput output;

  if (node.IsScalar()) {
    output.exact = node.as<std::string>();
    return output;
  }

  if (!node.IsMap()) {
    throw std::runtime_error(
        std::format(
            "{}: {} must be a string or map with contains/not_contains",
            FormatLocation(file_path, node.Mark()), context));
  }

  ValidateKeys(node, {"contains", "not_contains"}, context, file_path);

  if (node["contains"]) {
    ValidateIsSequence(node["contains"], "contains", context, file_path);
    for (const auto& item : node["contains"]) {
      output.contains.push_back(item.as<std::string>());
    }
  }

  if (node["not_contains"]) {
    ValidateIsSequence(
        node["not_contains"], "not_contains", context, file_path);
    for (const auto& item : node["not_contains"]) {
      output.not_contains.push_back(item.as<std::string>());
    }
  }

  return output;
}

}  // namespace

auto LoadTestCasesFromYaml(const std::string& path) -> std::vector<TestCase> {
  std::vector<TestCase> cases;

  auto root = YAML::LoadFile(path);
  ValidateKeys(root, {"feature", "description", "cases"}, "root", path);

  // Validate 'cases' exists and is a sequence
  if (!root["cases"]) {
    throw std::runtime_error(
        std::format("{}: missing required 'cases' field", path));
  }
  if (!root["cases"].IsSequence()) {
    throw std::runtime_error(
        std::format(
            "{}: 'cases' must be a list",
            FormatLocation(path, root["cases"].Mark())));
  }

  // Validate optional fields are scalars
  ValidateIsScalar(root["feature"], "feature", "root", path);
  ValidateIsScalar(root["description"], "description", "root", path);

  auto feature = root["feature"].as<std::string>("");

  for (const auto& node : root["cases"]) {
    TestCase test_case;

    // Validate and extract name
    if (!node["name"]) {
      throw std::runtime_error(
          std::format(
              "{}: test case missing required 'name' field",
              FormatLocation(path, node.Mark())));
    }
    ValidateIsScalar(node["name"], "name", "case", path);
    test_case.name = node["name"].as<std::string>();
    if (test_case.name.empty()) {
      throw std::runtime_error(
          std::format(
              "{}: test case name cannot be empty",
              FormatLocation(path, node["name"].Mark())));
    }

    test_case.feature = feature;
    auto case_context = std::format("case '{}'", test_case.name);

    ValidateKeys(
        node, {"name", "description", "sv", "files", "plusargs", "expect"},
        case_context, path);

    // Single-file format: sv: |
    if (node["sv"]) {
      ValidateIsScalar(node["sv"], "sv", case_context, path);
      test_case.sv_code = node["sv"].as<std::string>();
    }

    // Multi-file format: files: [...]
    if (node["files"]) {
      ValidateIsSequence(node["files"], "files", case_context, path);
      for (const auto& file_node : node["files"]) {
        ValidateKeys(
            file_node, {"name", "content"},
            std::format("files entry in {}", case_context), path);
        SourceFile source_file;
        source_file.name = file_node["name"].as<std::string>();
        source_file.content = file_node["content"].as<std::string>();
        test_case.files.push_back(std::move(source_file));
      }
    }

    // Plusargs: plusargs: ["+VERBOSE", "+COUNT=42"]
    if (node["plusargs"]) {
      ValidateIsSequence(node["plusargs"], "plusargs", case_context, path);
      for (const auto& arg : node["plusargs"]) {
        test_case.plusargs.push_back(arg.as<std::string>());
      }
    }

    // Parse unified expect: block
    if (node["expect"]) {
      const auto& expect = node["expect"];
      auto expect_context = std::format("expect in {}", case_context);
      ValidateKeys(
          expect, {"variables", "time", "stdout", "files"}, expect_context,
          path);

      // expect.variables: {var: value, ...}
      if (expect["variables"]) {
        for (const auto& pair : expect["variables"]) {
          auto var_name = pair.first.as<std::string>();
          test_case.expected_values[var_name] = ParseNumericValue(pair.second);
        }
      }

      // expect.time: N
      if (expect["time"]) {
        test_case.expected_time = expect["time"].as<uint64_t>();
      }

      // expect.stdout: string OR {contains: [...], not_contains: [...]}
      if (expect["stdout"]) {
        test_case.expected_stdout = ParseExpectedOutput(
            expect["stdout"], std::format("expect.stdout in {}", case_context),
            path);
      }

      // expect.files: {filename: content_or_spec, ...}
      if (expect["files"]) {
        for (const auto& pair : expect["files"]) {
          auto raw_filename = pair.first.as<std::string>();
          auto filename = ValidateAndNormalizeFilename(
              raw_filename, pair.first.Mark(), path,
              std::format("expect.files in {}", case_context));

          auto file_context =
              std::format("expect.files.{} in {}", filename, case_context);
          test_case.expected_files[filename] =
              ParseExpectedOutput(pair.second, file_context, path);
        }
      }
    }

    cases.push_back(std::move(test_case));
  }

  return cases;
}

}  // namespace lyra::test
