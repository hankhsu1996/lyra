#pragma once

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::common::mem_io {

inline auto ResolveMemPath(std::string_view filename) -> std::filesystem::path {
  std::filesystem::path path{std::string(filename)};
  if (path.is_relative()) {
    path = std::filesystem::current_path() / path;
  }
  return path;
}

// Parse a single digit from a memory file token.
// Returns -1 for underscore (separator), digit value otherwise.
inline auto ParseMemDigit(char ch, bool is_hex)
    -> std::expected<int, std::string> {
  if (ch == '_') {
    return -1;
  }
  if (!is_hex) {
    if (ch == '0') {
      return 0;
    }
    if (ch == '1') {
      return 1;
    }
    if (ch == 'x' || ch == 'X' || ch == 'z' || ch == 'Z') {
      return std::unexpected("readmem does not support X/Z digits");
    }
    return std::unexpected(std::format("invalid binary digit: {}", ch));
  }
  if (ch >= '0' && ch <= '9') {
    return ch - '0';
  }
  if (ch >= 'a' && ch <= 'f') {
    return 10 + (ch - 'a');
  }
  if (ch >= 'A' && ch <= 'F') {
    return 10 + (ch - 'A');
  }
  if (ch == 'x' || ch == 'X' || ch == 'z' || ch == 'Z') {
    return std::unexpected("readmem does not support X/Z digits");
  }
  return std::unexpected(std::format("invalid hex digit: {}", ch));
}

// Parse an address token from a memory file.
inline auto ParseMemAddress(std::string_view token, bool is_hex)
    -> std::expected<uint64_t, std::string> {
  if (token.empty()) {
    return std::unexpected("readmem address token is empty");
  }
  uint64_t value = 0;
  size_t bit_pos = 0;
  int bits_per_digit = is_hex ? 4 : 1;
  for (size_t i = token.size(); i > 0; --i) {
    auto digit_result = ParseMemDigit(token[i - 1], is_hex);
    if (!digit_result) {
      return std::unexpected(digit_result.error());
    }
    int digit = *digit_result;
    if (digit < 0) {
      continue;
    }
    for (int b = 0; b < bits_per_digit; ++b) {
      if ((digit & (1 << b)) != 0) {
        if (bit_pos >= 64) {
          return std::unexpected("readmem address exceeds 64-bit range");
        }
        value |= (1ULL << bit_pos);
      }
      ++bit_pos;
    }
  }
  return value;
}

// Parse a value token into 64-bit words.
inline auto ParseMemTokenToWords(
    std::string_view token, size_t bit_width, bool is_hex)
    -> std::expected<std::vector<uint64_t>, std::string> {
  if (token.empty()) {
    return std::unexpected("readmem value token is empty");
  }
  size_t word_count = (bit_width + 63) / 64;
  std::vector<uint64_t> words(word_count, 0);
  size_t bit_pos = 0;
  int bits_per_digit = is_hex ? 4 : 1;
  for (size_t i = token.size(); i > 0; --i) {
    auto digit_result = ParseMemDigit(token[i - 1], is_hex);
    if (!digit_result) {
      return std::unexpected(digit_result.error());
    }
    int digit = *digit_result;
    if (digit < 0) {
      continue;
    }
    for (int b = 0; b < bits_per_digit; ++b) {
      if (bit_pos >= bit_width) {
        break;
      }
      if ((digit & (1 << b)) != 0) {
        size_t word = bit_pos / 64;
        size_t bit = bit_pos % 64;
        words[word] |= (1ULL << bit);
      }
      ++bit_pos;
    }
    if (bit_pos >= bit_width) {
      break;
    }
  }
  return words;
}

// Format words as a hex or binary string.
inline auto FormatMemWords(
    const std::vector<uint64_t>& words, size_t bit_width, bool is_hex)
    -> std::string {
  if (bit_width == 0) {
    return "0";
  }
  std::string result;
  if (is_hex) {
    size_t digits = (bit_width + 3) / 4;
    result.resize(digits, '0');
    for (size_t d = 0; d < digits; ++d) {
      size_t nibble = digits - 1 - d;
      int val = 0;
      for (int b = 0; b < 4; ++b) {
        size_t bit_index = (nibble * 4) + static_cast<size_t>(b);
        if (bit_index >= bit_width) {
          continue;
        }
        size_t word = bit_index / 64;
        size_t bit = bit_index % 64;
        bool bit_val = ((words[word] >> bit) & 1ULL) != 0;
        if (bit_val) {
          val |= (1 << b);
        }
      }
      constexpr std::array<char, 16> kHexDigits = {'0', '1', '2', '3', '4', '5',
                                                   '6', '7', '8', '9', 'a', 'b',
                                                   'c', 'd', 'e', 'f'};
      result[d] = kHexDigits.at(static_cast<size_t>(val));
    }
    return result;
  }

  result.resize(bit_width, '0');
  for (size_t i = 0; i < bit_width; ++i) {
    size_t bit_index = bit_width - 1 - i;
    size_t word = bit_index / 64;
    size_t bit = bit_index % 64;
    bool bit_val = ((words[word] >> bit) & 1ULL) != 0;
    result[i] = bit_val ? '1' : '0';
  }
  return result;
}

// Result of parsing a memory file - either success or error message.
struct ParseMemFileResult {
  bool success = true;
  std::string error;

  static auto Success() -> ParseMemFileResult {
    return {.success = true, .error = {}};
  }
  static auto Error(std::string msg) -> ParseMemFileResult {
    return {.success = false, .error = std::move(msg)};
  }
};

// Parse a memory file and call store for each value.
// Returns error if parsing fails. The store callback receives (token, address).
template <typename StoreElementFn>
inline auto ParseMemFile(
    std::string_view content, bool is_hex, int64_t min_addr, int64_t max_addr,
    int64_t& current_addr, int64_t final_addr, std::string_view task_name,
    const StoreElementFn& store) -> ParseMemFileResult {
  size_t i = 0;
  while (i < content.size() && current_addr <= final_addr) {
    char ch = content[i];
    if (std::isspace(static_cast<unsigned char>(ch)) != 0) {
      ++i;
      continue;
    }
    if (ch == '/' && i + 1 < content.size()) {
      if (content[i + 1] == '/') {
        i += 2;
        while (i < content.size() && content[i] != '\n') {
          ++i;
        }
        continue;
      }
      if (content[i + 1] == '*') {
        i += 2;
        while (i + 1 < content.size() &&
               (content[i] != '*' || content[i + 1] != '/')) {
          ++i;
        }
        i = std::min(i + 2, content.size());
        continue;
      }
    }
    if (ch == '@') {
      ++i;
      size_t start = i;
      while (i < content.size() &&
             std::isspace(static_cast<unsigned char>(content[i])) == 0) {
        if (content[i] == '/' && i + 1 < content.size() &&
            (content[i + 1] == '/' || content[i + 1] == '*')) {
          break;
        }
        ++i;
      }
      auto token = std::string_view(content).substr(start, i - start);
      auto addr_result = ParseMemAddress(token, is_hex);
      if (!addr_result) {
        return ParseMemFileResult::Error(addr_result.error());
      }
      current_addr = static_cast<int64_t>(*addr_result);
      if (current_addr < min_addr || current_addr > max_addr) {
        return ParseMemFileResult::Error(
            std::format("{} address directive out of bounds", task_name));
      }
      continue;
    }

    size_t start = i;
    while (i < content.size() &&
           std::isspace(static_cast<unsigned char>(content[i])) == 0) {
      if (content[i] == '/' && i + 1 < content.size() &&
          (content[i + 1] == '/' || content[i + 1] == '*')) {
        break;
      }
      ++i;
    }
    auto token = std::string_view(content).substr(start, i - start);
    store(token, current_addr);
    ++current_addr;
  }
  return ParseMemFileResult::Success();
}

}  // namespace lyra::common::mem_io
