#pragma once

#include <algorithm>
#include <cstdint>
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

template <typename ErrorFn>
inline auto ParseMemDigit(char ch, bool is_hex, ErrorFn&& on_error) -> int {
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
      on_error("readmem does not support X/Z digits");
    }
    on_error(std::format("invalid binary digit: {}", ch));
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
    on_error("readmem does not support X/Z digits");
  }
  on_error(std::format("invalid hex digit: {}", ch));
}

template <typename ErrorFn>
inline auto ParseMemAddress(
    std::string_view token, bool is_hex, ErrorFn&& on_error) -> uint64_t {
  if (token.empty()) {
    on_error("readmem address token is empty");
  }
  uint64_t value = 0;
  size_t bit_pos = 0;
  int bits_per_digit = is_hex ? 4 : 1;
  for (size_t i = token.size(); i > 0; --i) {
    int digit = ParseMemDigit(token[i - 1], is_hex, on_error);
    if (digit < 0) {
      continue;
    }
    for (int b = 0; b < bits_per_digit; ++b) {
      if (digit & (1 << b)) {
        if (bit_pos >= 64) {
          on_error("readmem address exceeds 64-bit range");
        }
        value |= (1ULL << bit_pos);
      }
      ++bit_pos;
    }
  }
  return value;
}

template <typename ErrorFn>
inline auto ParseMemTokenToWords(
    std::string_view token, size_t bit_width, bool is_hex, ErrorFn&& on_error)
    -> std::vector<uint64_t> {
  if (token.empty()) {
    on_error("readmem value token is empty");
  }
  size_t word_count = (bit_width + 63) / 64;
  std::vector<uint64_t> words(word_count, 0);
  size_t bit_pos = 0;
  int bits_per_digit = is_hex ? 4 : 1;
  for (size_t i = token.size(); i > 0; --i) {
    int digit = ParseMemDigit(token[i - 1], is_hex, on_error);
    if (digit < 0) {
      continue;
    }
    for (int b = 0; b < bits_per_digit; ++b) {
      if (bit_pos >= bit_width) {
        break;
      }
      if (digit & (1 << b)) {
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
        size_t bit_index = nibble * 4 + b;
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
      result[d] = "0123456789abcdef"[val];
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

inline auto FormatMemAddress(uint64_t address, bool is_hex) -> std::string {
  if (is_hex) {
    return std::format("{:x}", address);
  }
  if (address == 0) {
    return "0";
  }
  std::string out;
  while (address != 0) {
    out.push_back((address & 1ULL) ? '1' : '0');
    address >>= 1U;
  }
  std::reverse(out.begin(), out.end());
  return out;
}

}  // namespace lyra::common::mem_io
