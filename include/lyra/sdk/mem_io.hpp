#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/sdk/bit.hpp"
#include "lyra/sdk/wide_bit.hpp"

namespace lyra::sdk {
namespace detail {

[[noreturn]] inline void ThrowMemIoError(const std::string& message) {
  throw std::runtime_error(message);
}

inline auto ResolveMemPath(std::string_view filename) -> std::filesystem::path {
  std::filesystem::path path{std::string(filename)};
  if (path.is_relative()) {
    if (const char* env = std::getenv("LYRA_PROJECT_ROOT")) {
      path = std::filesystem::path(env) / path;
    } else {
      path = std::filesystem::current_path() / path;
    }
  }
  return path;
}

inline auto ParseMemDigit(char ch, bool is_hex) -> int {
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
      ThrowMemIoError("readmem does not support X/Z digits");
    }
    ThrowMemIoError(std::string("invalid binary digit: ") + ch);
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
    ThrowMemIoError("readmem does not support X/Z digits");
  }
  ThrowMemIoError(std::string("invalid hex digit: ") + ch);
}

inline auto ParseMemAddress(std::string_view token, bool is_hex) -> uint64_t {
  if (token.empty()) {
    ThrowMemIoError("readmem address token is empty");
  }
  uint64_t value = 0;
  size_t bit_pos = 0;
  int bits_per_digit = is_hex ? 4 : 1;
  for (size_t i = token.size(); i > 0; --i) {
    int digit = ParseMemDigit(token[i - 1], is_hex);
    if (digit < 0) {
      continue;
    }
    for (int b = 0; b < bits_per_digit; ++b) {
      if (digit & (1 << b)) {
        if (bit_pos >= 64) {
          ThrowMemIoError("readmem address exceeds 64-bit range");
        }
        value |= (1ULL << bit_pos);
      }
      ++bit_pos;
    }
  }
  return value;
}

inline auto ParseMemTokenToWords(
    std::string_view token, size_t bit_width, bool is_hex)
    -> std::vector<uint64_t> {
  if (token.empty()) {
    ThrowMemIoError("readmem value token is empty");
  }
  size_t word_count = (bit_width + 63) / 64;
  std::vector<uint64_t> words(word_count, 0);
  size_t bit_pos = 0;
  int bits_per_digit = is_hex ? 4 : 1;
  for (size_t i = token.size(); i > 0; --i) {
    int digit = ParseMemDigit(token[i - 1], is_hex);
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

template <typename ElemT>
auto ParseMemTokenToElement(std::string_view token, bool is_hex) -> ElemT {
  constexpr size_t kWidth = ElemT::kWidth;
  auto words = ParseMemTokenToWords(token, kWidth, is_hex);
  if constexpr (kWidth <= 64) {
    uint64_t value = words.empty() ? 0 : words[0];
    return ElemT{value};
  } else {
    ElemT value{};
    for (size_t i = 0; i < words.size(); ++i) {
      value.SetWord(i, words[i]);
    }
    return value;
  }
}

template <typename ElemT>
auto GetElemBit(const ElemT& value, size_t bit_index) -> bool {
  if constexpr (ElemT::kWidth <= 64) {
    return ((static_cast<uint64_t>(value.Value()) >> bit_index) & 1ULL) != 0;
  } else {
    size_t word = bit_index / 64;
    size_t bit = bit_index % 64;
    return ((value.GetWord(word) >> bit) & 1ULL) != 0;
  }
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

template <typename ElemT>
auto FormatMemValue(const ElemT& value, bool is_hex) -> std::string {
  constexpr size_t kWidth = ElemT::kWidth;
  std::vector<uint64_t> words((kWidth + 63) / 64, 0);
  if constexpr (kWidth <= 64) {
    if (!words.empty()) {
      words[0] = static_cast<uint64_t>(value.Value());
    }
  } else {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = value.GetWord(i);
    }
  }
  return FormatMemWords(words, kWidth, is_hex);
}

template <typename PackedT>
auto GetPackedBit(const PackedT& value, size_t bit_index) -> bool {
  if constexpr (PackedT::kWidth <= 64) {
    return ((static_cast<uint64_t>(value.Value()) >> bit_index) & 1ULL) != 0;
  } else {
    size_t word = bit_index / 64;
    size_t bit = bit_index % 64;
    return ((value.GetWord(word) >> bit) & 1ULL) != 0;
  }
}

template <typename PackedT>
auto SetPackedBit(PackedT value, size_t bit_index, bool bit) -> PackedT {
  if constexpr (PackedT::kWidth <= 64) {
    uint64_t raw = static_cast<uint64_t>(value.Value());
    uint64_t mask = 1ULL << bit_index;
    if (bit) {
      raw |= mask;
    } else {
      raw &= ~mask;
    }
    return PackedT{raw};
  } else {
    size_t word = bit_index / 64;
    size_t bit_pos = bit_index % 64;
    uint64_t raw = value.GetWord(word);
    uint64_t mask = 1ULL << bit_pos;
    if (bit) {
      raw |= mask;
    } else {
      raw &= ~mask;
    }
    value.SetWord(word, raw);
    return value;
  }
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

}  // namespace detail

template <typename ArrayT>
auto ReadMemArray(
    ArrayT& array, int32_t lower_bound, std::string_view filename,
    bool has_start, int64_t start, bool has_end, int64_t end, bool is_hex)
    -> void {
  using ElemT = typename ArrayT::value_type;
  int64_t min_addr = lower_bound;
  int64_t max_addr = lower_bound + static_cast<int64_t>(array.size()) - 1;
  int64_t current_addr = has_start ? start : min_addr;
  int64_t final_addr = has_end ? end : max_addr;

  if (array.empty()) {
    detail::ThrowMemIoError("readmem target has zero size");
  }
  if (current_addr < min_addr || current_addr > max_addr) {
    detail::ThrowMemIoError("readmem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    detail::ThrowMemIoError("readmem end address out of bounds");
  }

  auto path = detail::ResolveMemPath(filename);
  std::ifstream in(path);
  if (!in) {
    detail::ThrowMemIoError("failed to open memory file: " + path.string());
  }
  std::string content(
      (std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());

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
               !(content[i] == '*' && content[i + 1] == '/')) {
          ++i;
        }
        i = std::min(i + 2, content.size());
        continue;
      }
    }
    if (ch == '@') {
      ++i;
      size_t start_idx = i;
      while (i < content.size() &&
             std::isspace(static_cast<unsigned char>(content[i])) == 0) {
        if (content[i] == '/' && i + 1 < content.size() &&
            (content[i + 1] == '/' || content[i + 1] == '*')) {
          break;
        }
        ++i;
      }
      auto token = std::string_view(content).substr(start_idx, i - start_idx);
      uint64_t addr = detail::ParseMemAddress(token, is_hex);
      current_addr = static_cast<int64_t>(addr);
      if (current_addr < min_addr || current_addr > max_addr) {
        detail::ThrowMemIoError("readmem address directive out of bounds");
      }
      continue;
    }

    size_t start_idx = i;
    while (i < content.size() &&
           std::isspace(static_cast<unsigned char>(content[i])) == 0) {
      if (content[i] == '/' && i + 1 < content.size() &&
          (content[i + 1] == '/' || content[i + 1] == '*')) {
        break;
      }
      ++i;
    }
    auto token = std::string_view(content).substr(start_idx, i - start_idx);
    auto value = detail::ParseMemTokenToElement<ElemT>(token, is_hex);
    size_t index = static_cast<size_t>(current_addr - min_addr);
    array[index] = value;
    ++current_addr;
  }
}

template <typename PackedT>
auto ReadMemPacked(
    PackedT& packed, size_t element_width, size_t element_count,
    int32_t lower_bound, std::string_view filename, bool has_start,
    int64_t start, bool has_end, int64_t end, bool is_hex) -> void {
  if (element_width == 0 || element_count == 0) {
    detail::ThrowMemIoError("readmem target has zero size");
  }
  int64_t min_addr = lower_bound;
  int64_t max_addr = lower_bound + static_cast<int64_t>(element_count) - 1;
  int64_t current_addr = has_start ? start : min_addr;
  int64_t final_addr = has_end ? end : max_addr;

  if (current_addr < min_addr || current_addr > max_addr) {
    detail::ThrowMemIoError("readmem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    detail::ThrowMemIoError("readmem end address out of bounds");
  }

  auto path = detail::ResolveMemPath(filename);
  std::ifstream in(path);
  if (!in) {
    detail::ThrowMemIoError("failed to open memory file: " + path.string());
  }
  std::string content(
      (std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());

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
               !(content[i] == '*' && content[i + 1] == '/')) {
          ++i;
        }
        i = std::min(i + 2, content.size());
        continue;
      }
    }
    if (ch == '@') {
      ++i;
      size_t start_idx = i;
      while (i < content.size() &&
             std::isspace(static_cast<unsigned char>(content[i])) == 0) {
        if (content[i] == '/' && i + 1 < content.size() &&
            (content[i + 1] == '/' || content[i + 1] == '*')) {
          break;
        }
        ++i;
      }
      auto token = std::string_view(content).substr(start_idx, i - start_idx);
      uint64_t addr = detail::ParseMemAddress(token, is_hex);
      current_addr = static_cast<int64_t>(addr);
      if (current_addr < min_addr || current_addr > max_addr) {
        detail::ThrowMemIoError("readmem address directive out of bounds");
      }
      continue;
    }

    size_t start_idx = i;
    while (i < content.size() &&
           std::isspace(static_cast<unsigned char>(content[i])) == 0) {
      if (content[i] == '/' && i + 1 < content.size() &&
          (content[i + 1] == '/' || content[i + 1] == '*')) {
        break;
      }
      ++i;
    }
    auto token = std::string_view(content).substr(start_idx, i - start_idx);
    auto elem_words =
        detail::ParseMemTokenToWords(token, element_width, is_hex);
    size_t index = static_cast<size_t>(current_addr - min_addr);
    size_t base_bit = index * element_width;
    for (size_t bit = 0; bit < element_width; ++bit) {
      size_t word = bit / 64;
      size_t bit_pos = bit % 64;
      bool bit_val = ((elem_words[word] >> bit_pos) & 1ULL) != 0;
      packed = detail::SetPackedBit(packed, base_bit + bit, bit_val);
    }
    ++current_addr;
  }
}

template <typename ArrayT>
auto WriteMemArray(
    const ArrayT& array, int32_t lower_bound, std::string_view filename,
    bool has_start, int64_t start, bool has_end, int64_t end, bool is_hex)
    -> void {
  int64_t min_addr = lower_bound;
  int64_t max_addr = lower_bound + static_cast<int64_t>(array.size()) - 1;
  int64_t current_addr = has_start ? start : min_addr;
  int64_t final_addr = has_end ? end : max_addr;

  if (array.empty()) {
    detail::ThrowMemIoError("writemem target has zero size");
  }
  if (current_addr < min_addr || current_addr > max_addr) {
    detail::ThrowMemIoError("writemem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    detail::ThrowMemIoError("writemem end address out of bounds");
  }

  auto path = detail::ResolveMemPath(filename);
  std::ofstream out(path);
  if (!out) {
    detail::ThrowMemIoError(
        "failed to open memory file for write: " + path.string());
  }

  if (has_start) {
    out << "@"
        << detail::FormatMemAddress(static_cast<uint64_t>(current_addr), is_hex)
        << "\n";
  }

  for (int64_t addr = current_addr; addr <= final_addr; ++addr) {
    size_t index = static_cast<size_t>(addr - min_addr);
    out << detail::FormatMemValue(array[index], is_hex) << "\n";
  }
}

template <typename PackedT>
auto WriteMemPacked(
    const PackedT& packed, size_t element_width, size_t element_count,
    int32_t lower_bound, std::string_view filename, bool has_start,
    int64_t start, bool has_end, int64_t end, bool is_hex) -> void {
  if (element_width == 0 || element_count == 0) {
    detail::ThrowMemIoError("writemem target has zero size");
  }
  int64_t min_addr = lower_bound;
  int64_t max_addr = lower_bound + static_cast<int64_t>(element_count) - 1;
  int64_t current_addr = has_start ? start : min_addr;
  int64_t final_addr = has_end ? end : max_addr;

  if (current_addr < min_addr || current_addr > max_addr) {
    detail::ThrowMemIoError("writemem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    detail::ThrowMemIoError("writemem end address out of bounds");
  }

  auto path = detail::ResolveMemPath(filename);
  std::ofstream out(path);
  if (!out) {
    detail::ThrowMemIoError(
        "failed to open memory file for write: " + path.string());
  }

  if (has_start) {
    out << "@"
        << detail::FormatMemAddress(static_cast<uint64_t>(current_addr), is_hex)
        << "\n";
  }

  for (int64_t addr = current_addr; addr <= final_addr; ++addr) {
    size_t index = static_cast<size_t>(addr - min_addr);
    size_t base_bit = index * element_width;
    auto elem_words = std::vector<uint64_t>((element_width + 63) / 64, 0);
    for (size_t bit = 0; bit < element_width; ++bit) {
      bool bit_val = detail::GetPackedBit(packed, base_bit + bit);
      if (bit_val) {
        size_t word = bit / 64;
        size_t bit_pos = bit % 64;
        elem_words[word] |= (1ULL << bit_pos);
      }
    }
    out << detail::FormatMemWords(elem_words, element_width, is_hex) << "\n";
  }
}

}  // namespace lyra::sdk
