#include "tests/framework/test_value.hpp"

#include <cctype>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string_view>
#include <vector>

namespace lyra::test {

namespace {

auto Trim(std::string_view str) -> std::string_view {
  while (!str.empty() &&
         std::isspace(static_cast<unsigned char>(str.front())) != 0) {
    str.remove_prefix(1);
  }
  while (!str.empty() &&
         std::isspace(static_cast<unsigned char>(str.back())) != 0) {
    str.remove_suffix(1);
  }
  return str;
}

}  // namespace

// Parse SV literal format: N'b..., N'h..., N'o... into IntegralValue
// Returns std::nullopt if not in SV literal format.
// Enforces exact width matching: effective_bits must not exceed width (no
// truncation). Zero-extension (fewer bits than width) is allowed.
auto ParseSvLiteral(std::string_view str) -> std::optional<IntegralValue> {
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

}  // namespace lyra::test
