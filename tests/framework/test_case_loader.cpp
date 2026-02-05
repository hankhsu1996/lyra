#include "tests/framework/test_case_loader.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <filesystem>
#include <format>
#include <initializer_list>
#include <optional>
#include <ranges>
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

// Trim whitespace from both ends of a string
auto Trim(std::string_view sv) -> std::string_view {
  while (!sv.empty() &&
         std::isspace(static_cast<unsigned char>(sv.front())) != 0) {
    sv.remove_prefix(1);
  }
  while (!sv.empty() &&
         std::isspace(static_cast<unsigned char>(sv.back())) != 0) {
    sv.remove_suffix(1);
  }
  return sv;
}

// Parse C-style integer format: 0x..., 0b..., 0o..., or decimal
// Returns std::nullopt if not a valid integer or if negative.
// Supports underscore separators (e.g., "0xff_ff").
auto ParseCStyleInteger(const std::string& str) -> std::optional<uint64_t> {
  std::string_view sv = Trim(str);
  if (sv.empty()) {
    return std::nullopt;
  }

  // Reject negative values
  if (sv[0] == '-') {
    return std::nullopt;
  }

  // Skip optional leading '+'
  if (sv[0] == '+') {
    sv.remove_prefix(1);
    if (sv.empty()) {
      return std::nullopt;
    }
  }

  int base = 10;
  if (sv.size() >= 2 && sv[0] == '0') {
    char c = static_cast<char>(std::tolower(static_cast<unsigned char>(sv[1])));
    if (c == 'x') {
      base = 16;
      sv.remove_prefix(2);
    } else if (c == 'b') {
      base = 2;
      sv.remove_prefix(2);
    } else if (c == 'o') {
      base = 8;
      sv.remove_prefix(2);
    }
  }

  if (sv.empty()) {
    return std::nullopt;
  }

  uint64_t result = 0;
  bool has_digit = false;
  for (char c : sv) {
    // Skip underscores (e.g., "0xff_ff")
    if (c == '_') {
      continue;
    }

    int digit = -1;
    if (c >= '0' && c <= '9') {
      digit = c - '0';
    } else {
      char lower =
          static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
      if (lower >= 'a' && lower <= 'f') {
        digit = 10 + (lower - 'a');
      }
    }

    if (digit < 0 || digit >= base) {
      return std::nullopt;  // Invalid digit for this base
    }

    // Check for overflow
    if (result > (UINT64_MAX - digit) / base) {
      return std::nullopt;  // Overflow
    }
    result = result * base + digit;
    has_digit = true;
  }

  if (!has_digit) {
    return std::nullopt;
  }

  return result;
}

// Parse numeric value: integer, double, or SV literal
// Note: ParseSvLiteral is defined in test_value.cpp
auto ParseNumericValue(const YAML::Node& node) -> ExpectedValue {
  auto str = node.as<std::string>();

  // Try SV literal format: N'b..., N'h..., N'o...
  if (auto sv_literal = ParseSvLiteral(str)) {
    return *sv_literal;
  }

  // Try C-style integer format: 0x..., 0b..., 0o..., or decimal
  if (auto c_int = ParseCStyleInteger(str)) {
    return *c_int;
  }

  // Check for floating point
  bool has_decimal = str.find('.') != std::string::npos;
  bool has_exponent =
      str.find('e') != std::string::npos || str.find('E') != std::string::npos;

  if (has_decimal || has_exponent) {
    return node.as<double>();
  }

  // Fallback: use YAML's int64 parsing
  // Negative values are stored as-is; at comparison time they are reinterpreted
  // as 2's complement at the actual's width.
  auto int_val = node.as<int64_t>();
  return static_cast<uint64_t>(int_val);
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
        node,
        {"name", "description", "sv", "files", "plusargs", "param_overrides",
         "pedantic", "trace", "dump_slot_meta", "expect"},
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

    // Param overrides: param_overrides: ["WIDTH=32", "DEPTH=64"]
    if (node["param_overrides"]) {
      ValidateIsSequence(
          node["param_overrides"], "param_overrides", case_context, path);
      for (const auto& arg : node["param_overrides"]) {
        test_case.param_overrides.push_back(arg.as<std::string>());
      }
    }

    // Pedantic mode
    if (node["pedantic"]) {
      test_case.pedantic = node["pedantic"].as<bool>();
    }

    // Trace mode
    if (node["trace"]) {
      test_case.trace = node["trace"].as<bool>();
    }

    // Dump slot metadata (test-only)
    if (node["dump_slot_meta"]) {
      test_case.dump_slot_meta = node["dump_slot_meta"].as<bool>();
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
