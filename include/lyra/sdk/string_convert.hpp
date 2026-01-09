#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <type_traits>

#include "lyra/sdk/bit.hpp"
#include "lyra/sdk/wide_bit.hpp"

namespace lyra::sdk {

// String replication: creates count copies of the input string.
// Used for string replication operator {n{s}} per LRM 11.4.12.2.
inline auto ReplicateString(const std::string& s, size_t count) -> std::string {
  std::string result;
  result.reserve(s.size() * count);
  for (size_t i = 0; i < count; ++i) {
    result += s;
  }
  return result;
}

// Integral to string conversion per LRM 6.16.
// Each 8 bits forms one character, MSB first, null bytes are skipped.

// Overload for Bit<N, S> - use kWidth instead of sizeof
template <std::size_t Width, bool Signed>
auto IntToString(const Bit<Width, Signed>& value) -> std::string {
  std::string result;
  auto bits = static_cast<uint64_t>(value);
  for (size_t i = Width; i >= 8; i -= 8) {
    auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
    if (ch != 0) {
      result += static_cast<char>(ch);
    }
  }
  return result;
}

// Overload for NarrowConcatResult<N> - result of narrow Concat operations
template <std::size_t Width>
auto IntToString(const detail::NarrowConcatResult<Width>& value)
    -> std::string {
  std::string result;
  auto bits = value.value;
  for (size_t i = Width; i >= 8; i -= 8) {
    auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
    if (ch != 0) {
      result += static_cast<char>(ch);
    }
  }
  return result;
}

// Overload for WideBit<N, S> - extract bytes from multi-word storage
template <std::size_t Width, bool Signed>
auto IntToString(const WideBit<Width, Signed>& value) -> std::string {
  std::string result;
  for (size_t i = Width; i >= 8; i -= 8) {
    size_t byte_start = i - 8;
    size_t word_idx = byte_start / 64;
    size_t bit_pos = byte_start % 64;
    auto ch = static_cast<uint8_t>((value.GetWord(word_idx) >> bit_pos) & 0xFF);
    if (ch != 0) {
      result += static_cast<char>(ch);
    }
  }
  return result;
}

// Generic overload for primitive integral types
template <typename T>
  requires std::is_integral_v<T>
auto IntToString(T value) -> std::string {
  std::string result;
  constexpr size_t kWidth = sizeof(T) * 8;
  auto bits = static_cast<uint64_t>(value);
  for (size_t i = kWidth; i >= 8; i -= 8) {
    auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
    if (ch != 0) {
      result += static_cast<char>(ch);
    }
  }
  return result;
}

}  // namespace lyra::sdk
