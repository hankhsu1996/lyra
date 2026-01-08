#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/mem_io.hpp"
#include "lyra/sdk/bit.hpp"
#include "lyra/sdk/wide_bit.hpp"

namespace lyra::sdk {
namespace detail {

template <typename ElemT>
auto ParseMemTokenToElement(std::string_view token, bool is_hex) -> ElemT {
  constexpr size_t kWidth = ElemT::kWidth;
  auto words = common::mem_io::ParseMemTokenToWords(
      token, kWidth, is_hex, [](std::string_view message) {
        throw std::runtime_error(std::string(message));
      });
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
  return common::mem_io::FormatMemWords(words, kWidth, is_hex);
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
    throw std::runtime_error("readmem target has zero size");
  }
  if (current_addr < min_addr || current_addr > max_addr) {
    throw std::runtime_error("readmem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    throw std::runtime_error("readmem end address out of bounds");
  }

  auto path = common::mem_io::ResolveMemPath(filename);
  std::ifstream in(path);
  if (!in) {
    throw std::runtime_error("failed to open memory file: " + path.string());
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
      uint64_t addr = common::mem_io::ParseMemAddress(
          token, is_hex, [](std::string_view message) {
            throw std::runtime_error(std::string(message));
          });
      current_addr = static_cast<int64_t>(addr);
      if (current_addr < min_addr || current_addr > max_addr) {
        throw std::runtime_error("readmem address directive out of bounds");
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
    throw std::runtime_error("readmem target has zero size");
  }
  int64_t min_addr = lower_bound;
  int64_t max_addr = lower_bound + static_cast<int64_t>(element_count) - 1;
  int64_t current_addr = has_start ? start : min_addr;
  int64_t final_addr = has_end ? end : max_addr;

  if (current_addr < min_addr || current_addr > max_addr) {
    throw std::runtime_error("readmem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    throw std::runtime_error("readmem end address out of bounds");
  }

  auto path = common::mem_io::ResolveMemPath(filename);
  std::ifstream in(path);
  if (!in) {
    throw std::runtime_error("failed to open memory file: " + path.string());
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
      uint64_t addr = common::mem_io::ParseMemAddress(
          token, is_hex, [](std::string_view message) {
            throw std::runtime_error(std::string(message));
          });
      current_addr = static_cast<int64_t>(addr);
      if (current_addr < min_addr || current_addr > max_addr) {
        throw std::runtime_error("readmem address directive out of bounds");
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
    auto elem_words = common::mem_io::ParseMemTokenToWords(
        token, element_width, is_hex, [](std::string_view message) {
          throw std::runtime_error(std::string(message));
        });
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
    throw std::runtime_error("writemem target has zero size");
  }
  if (current_addr < min_addr || current_addr > max_addr) {
    throw std::runtime_error("writemem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    throw std::runtime_error("writemem end address out of bounds");
  }

  auto path = common::mem_io::ResolveMemPath(filename);
  std::ofstream out(path);
  if (!out) {
    throw std::runtime_error(
        "failed to open memory file for write: " + path.string());
  }

  if (has_start) {
    out << "@"
        << common::mem_io::FormatMemAddress(
               static_cast<uint64_t>(current_addr), is_hex)
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
    throw std::runtime_error("writemem target has zero size");
  }
  int64_t min_addr = lower_bound;
  int64_t max_addr = lower_bound + static_cast<int64_t>(element_count) - 1;
  int64_t current_addr = has_start ? start : min_addr;
  int64_t final_addr = has_end ? end : max_addr;

  if (current_addr < min_addr || current_addr > max_addr) {
    throw std::runtime_error("writemem start address out of bounds");
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    throw std::runtime_error("writemem end address out of bounds");
  }

  auto path = common::mem_io::ResolveMemPath(filename);
  std::ofstream out(path);
  if (!out) {
    throw std::runtime_error(
        "failed to open memory file for write: " + path.string());
  }

  if (has_start) {
    out << "@"
        << common::mem_io::FormatMemAddress(
               static_cast<uint64_t>(current_addr), is_hex)
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
    out << common::mem_io::FormatMemWords(elem_words, element_width, is_hex)
        << "\n";
  }
}

}  // namespace lyra::sdk
