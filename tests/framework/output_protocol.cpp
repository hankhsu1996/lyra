#include "tests/framework/output_protocol.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <map>
#include <sstream>
#include <string>
#include <string_view>

#include "tests/framework/test_value.hpp"

namespace lyra::test {
namespace {

// Parse hex string into IntegralValue (unknown bits are all 0)
auto HexToIntegral(const std::string& hex, uint32_t width) -> IntegralValue {
  IntegralValue result;
  result.width = width;
  size_t num_words = (width + 63) / 64;
  result.value.resize(num_words, 0);
  result.unknown.resize(num_words, 0);  // No X/Z from LLVM backend

  // Parse hex string from LSB (rightmost) to MSB
  size_t hex_len = hex.size();
  for (size_t i = 0; i < hex_len; ++i) {
    char c = hex[hex_len - 1 - i];
    uint64_t nibble = 0;
    if (c >= '0' && c <= '9') {
      nibble = c - '0';
    } else if (c >= 'a' && c <= 'f') {
      nibble = 10 + (c - 'a');
    } else if (c >= 'A' && c <= 'F') {
      nibble = 10 + (c - 'A');
    }

    size_t bit_pos = i * 4;
    size_t word_idx = bit_pos / 64;
    size_t bit_in_word = bit_pos % 64;

    if (word_idx < num_words) {
      result.value[word_idx] |= nibble << bit_in_word;
    }
  }

  // Mask top word to actual width
  if (width > 0 && width % 64 != 0) {
    uint64_t mask = (uint64_t{1} << (width % 64)) - 1;
    result.value.back() &= mask;
  }

  return result;
}

// Parse a single __LYRA_VAR entry and extract variable value.
void ParseLyraVarEntry(
    std::string_view entry, std::map<std::string, TestValue>& variables) {
  // New format: "v:i:name=literal" or "v:r:name=value"
  // Old format (deprecated): "i:width:name=value" or "h:width:name=hex" or
  // "r:name=value"
  if (entry.size() < 2 || entry[1] != ':') {
    return;
  }

  char type_tag = entry[0];
  auto rest = entry.substr(2);  // Skip "X:"

  if (type_tag == 'v') {
    // New v: protocol: v:kind:name=literal
    // kind is 'i' for integral (SV literal), 'r' for real (plain decimal)
    if (rest.size() < 2 || rest[1] != ':') {
      return;
    }
    char var_kind = rest[0];
    auto name_literal = rest.substr(2);
    auto eq_pos = name_literal.find('=');
    if (eq_pos == std::string::npos) {
      return;
    }
    std::string name(name_literal.substr(0, eq_pos));
    std::string_view literal = name_literal.substr(eq_pos + 1);

    if (var_kind == 'i') {
      // Integral: parse SV literal (N'b... or N'h...)
      auto result = ParseSvLiteral(literal);
      if (result) {
        variables[name] = *result;
      }
    } else if (var_kind == 'r') {
      // Real: parse as double (not an SV literal)
      variables[name] = std::stod(std::string(literal));
    }
  } else if (type_tag == 'i' || type_tag == 'h') {
    // Old format: i:width:name=value or h:width:name=hex
    auto colon_pos = rest.find(':');
    if (colon_pos == std::string::npos) {
      return;
    }
    auto width = static_cast<uint32_t>(
        std::stoul(std::string(rest.substr(0, colon_pos))));
    auto name_value = rest.substr(colon_pos + 1);
    auto eq_pos = name_value.find('=');
    if (eq_pos == std::string::npos) {
      return;
    }
    std::string name(name_value.substr(0, eq_pos));
    std::string value_str(name_value.substr(eq_pos + 1));

    if (type_tag == 'i') {
      // Narrow integral - parse as unsigned and create IntegralValue
      uint64_t raw = std::stoull(value_str);
      IntegralValue fs;
      fs.width = width;
      fs.value = {raw};
      fs.unknown = {0};
      if (width > 0 && width < 64) {
        fs.value[0] &= (uint64_t{1} << width) - 1;
      }
      variables[name] = fs;
    } else {
      // Wide integral (hex)
      std::string hex(value_str);
      // Normalize to lowercase
      std::ranges::transform(hex, hex.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
      });
      variables[name] = HexToIntegral(hex, width);
    }
  } else if (type_tag == 'r') {
    // Old format: r:name=value (no width)
    auto eq_pos = rest.find('=');
    if (eq_pos != std::string::npos) {
      std::string name(rest.substr(0, eq_pos));
      std::string value_str(rest.substr(eq_pos + 1));
      variables[name] = std::stod(value_str);
    }
  }
}

}  // namespace

auto ParseLyraVarOutput(const std::string& output) -> ParsedOutput {
  ParsedOutput parsed;
  std::string& clean_output = parsed.clean;
  std::map<std::string, TestValue>& variables = parsed.variables;

  // Check if original output ends with newline - we need this to know
  // whether to add a trailing newline to the clean output
  bool output_ends_with_newline = !output.empty() && output.back() == '\n';

  std::istringstream stream(output);
  std::string line;
  bool first_clean_line = true;
  // Track whether user content should have trailing newline.
  // True if the last user content was on its own line (followed by \n),
  // false if user content was followed immediately by __LYRA_VAR (mid-line).
  bool user_ends_with_newline = false;
  constexpr std::string_view kPrefix = "__LYRA_VAR:";
  constexpr std::string_view kTimePrefix = "__LYRA_TIME__=";

  while (std::getline(stream, line)) {
    // Check for __LYRA_TIME__=<N> anywhere in the line
    auto time_pos = line.find(kTimePrefix);
    if (time_pos != std::string::npos) {
      parsed.final_time =
          std::stoull(std::string(line.substr(time_pos + kTimePrefix.size())));
      if (time_pos == 0) {
        // Line starts with __LYRA_TIME__= - no user content on this line
        continue;
      }
      // __LYRA_TIME__= appears mid-line (after $write output with no newline)
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line.substr(0, time_pos);
      first_clean_line = false;
      user_ends_with_newline = false;
      continue;
    }

    // Look for __LYRA_VAR: anywhere in the line
    auto var_pos = line.find(kPrefix);

    if (var_pos == std::string::npos) {
      // No __LYRA_VAR: in this line - add whole line to clean output
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line;
      first_clean_line = false;
      // Assume this line had user content and ended with \n, but we'll
      // correct this assumption at the end using output_ends_with_newline
      user_ends_with_newline = true;
    } else if (var_pos == 0) {
      // Line starts with __LYRA_VAR: - previous user content ended with \n
      // (Don't change user_ends_with_newline - keep previous value)
      ParseLyraVarEntry(line.substr(kPrefix.size()), variables);
    } else {
      // __LYRA_VAR: appears mid-line (after $write output with no newline)
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line.substr(0, var_pos);
      first_clean_line = false;
      // User content was NOT followed by newline (it was followed by
      // __LYRA_VAR)
      user_ends_with_newline = false;
      ParseLyraVarEntry(line.substr(var_pos + kPrefix.size()), variables);
    }
  }

  // Add trailing newline only if:
  // 1. User content should have one (user_ends_with_newline)
  // 2. There was some clean output (!first_clean_line)
  // 3. The original output actually ended with a newline
  if (user_ends_with_newline && !first_clean_line && output_ends_with_newline) {
    clean_output += '\n';
  }

  return parsed;
}

}  // namespace lyra::test
