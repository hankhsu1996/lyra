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

// Parse SV literal format: N'b..., N'h..., N'o... into IntegralValue
// Returns std::nullopt if not in SV literal format.
// Enforces exact width matching: effective_bits must not exceed width (no
// truncation). Zero-extension (fewer bits than width) is allowed.
auto ParseSvLiteral(const std::string& str) -> std::optional<IntegralValue> {
  // Trim whitespace
  std::string_view trimmed = Trim(str);
  if (trimmed.empty()) {
    return std::nullopt;
  }

  // Look for width'base pattern
  auto tick_pos = trimmed.find('\'');
  if (tick_pos == std::string_view::npos || tick_pos == 0 ||
      tick_pos + 1 >= trimmed.size()) {
    return std::nullopt;
  }

  // Parse width
  uint32_t width = 0;
  for (size_t i = 0; i < tick_pos; ++i) {
    if (trimmed[i] < '0' || trimmed[i] > '9') {
      return std::nullopt;
    }
    width = width * 10 + (trimmed[i] - '0');
  }
  if (width == 0) {
    return std::nullopt;
  }

  // Parse base
  char base = static_cast<char>(
      std::tolower(static_cast<unsigned char>(trimmed[tick_pos + 1])));
  if (base != 'b' && base != 'h' && base != 'o') {
    return std::nullopt;
  }

  std::string_view digits = trimmed.substr(tick_pos + 2);
  if (digits.empty()) {
    return std::nullopt;
  }

  // Determine bits per digit based on base
  uint32_t bits_per_digit = 4;  // hex default
  if (base == 'b') {
    bits_per_digit = 1;
  } else if (base == 'o') {
    bits_per_digit = 3;
  }

  // Count effective digits (excluding underscores) and check for truncation
  uint32_t digit_count = 0;
  for (char c : digits) {
    if (c != '_') {
      ++digit_count;
    }
  }
  uint32_t effective_bits = digit_count * bits_per_digit;
  if (effective_bits > width) {
    // Would truncate MSB bits - reject to enforce exact width matching
    return std::nullopt;
  }

  // Calculate number of words needed
  size_t num_words = (width + 63) / 64;

  IntegralValue result;
  result.width = width;
  result.value.resize(num_words, 0);
  result.unknown.resize(num_words, 0);

  // Parse digits MSB-first, storing into LSB-first word array
  // Track bit position from LSB (bit 0)
  uint32_t total_bits_parsed = 0;
  for (char digit : std::ranges::reverse_view(digits)) {
    char c = static_cast<char>(std::tolower(static_cast<unsigned char>(digit)));

    // Skip underscores (SV allows _ in literals)
    if (c == '_') {
      continue;
    }

    uint32_t digit_val = 0;
    uint32_t digit_unk = 0;

    if (c == 'x') {
      // All bits in this digit are X
      digit_unk = (1U << bits_per_digit) - 1;
      digit_val = 0;
    } else if (c == 'z') {
      // All bits in this digit are Z (don't accept '?' - keep expected values
      // unambiguous)
      digit_unk = (1U << bits_per_digit) - 1;
      digit_val = (1U << bits_per_digit) - 1;
    } else if (base == 'b' && (c == '0' || c == '1')) {
      digit_val = (c == '1') ? 1 : 0;
    } else if (base == 'o' && c >= '0' && c <= '7') {
      digit_val = c - '0';
    } else if (base == 'h') {
      if (c >= '0' && c <= '9') {
        digit_val = c - '0';
      } else if (c >= 'a' && c <= 'f') {
        digit_val = 10 + (c - 'a');
      } else {
        return std::nullopt;  // Invalid hex digit
      }
    } else {
      return std::nullopt;  // Invalid digit for this base
    }

    // Place each bit of this digit (zero-extension is OK, so no width check
    // here)
    for (uint32_t b = 0; b < bits_per_digit; ++b) {
      size_t word_idx = total_bits_parsed / 64;
      uint32_t bit_idx = total_bits_parsed % 64;

      if (((digit_val >> b) & 1) != 0) {
        result.value[word_idx] |= (uint64_t{1} << bit_idx);
      }
      if (((digit_unk >> b) & 1) != 0) {
        result.unknown[word_idx] |= (uint64_t{1} << bit_idx);
      }
      ++total_bits_parsed;
    }
  }

  // Mask high bits above width (sanity: ensure no garbage from larger digits)
  if (width % 64 != 0) {
    uint64_t mask = (uint64_t{1} << (width % 64)) - 1;
    result.value.back() &= mask;
    result.unknown.back() &= mask;
  }

  return result;
}

// Parse numeric value: integer, double, or SV literal
auto ParseNumericValue(const YAML::Node& node) -> ExpectedValue {
  auto str = node.as<std::string>();

  // Try SV literal format: N'b..., N'h..., N'o...
  if (auto sv_literal = ParseSvLiteral(str)) {
    return *sv_literal;
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
