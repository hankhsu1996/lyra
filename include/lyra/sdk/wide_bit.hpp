#pragma once

// SDK WideBit<N> - self-contained header for generated C++ code.
// This file is embedded into generated projects and must not depend on
// other Lyra headers. The algorithms here mirror those in
// lyra/common/wide_bit_ops.hpp but are duplicated for SDK independence.

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <ostream>
#include <string>

namespace lyra::sdk {

// Forward declaration for Bit class
template <std::size_t Width, bool Signed>
class Bit;

namespace detail {

// Calculate number of 64-bit words needed for a given bit width
constexpr auto WordsForBits(std::size_t bit_width) -> std::size_t {
  return (bit_width + 63) / 64;
}

// Mask for the final word given bit width
constexpr auto FinalWordMask(std::size_t bit_width) -> uint64_t {
  std::size_t bits_in_final = bit_width % 64;
  return (bits_in_final == 0) ? ~0ULL : (1ULL << bits_in_final) - 1;
}

// Check if all words are zero
template <typename Container>
constexpr auto IsZero(const Container& words, std::size_t num_words) -> bool {
  for (std::size_t i = 0; i < num_words; ++i) {
    if (words[i] != 0) {
      return false;
    }
  }
  return true;
}

// Divide multi-word integer by single-word divisor.
// Returns remainder; quotient is stored in words (modified in-place).
template <typename Container>
auto DivideByWord(Container& words, std::size_t num_words, uint64_t divisor)
    -> uint64_t {
  __uint128_t remainder = 0;
  for (std::size_t i = num_words; i > 0; --i) {
    __uint128_t dividend = (remainder << 64) | words[i - 1];
    words[i - 1] = static_cast<uint64_t>(dividend / divisor);
    remainder = dividend % divisor;
  }
  return static_cast<uint64_t>(remainder);
}

// Convert to decimal string (unsigned interpretation)
template <std::size_t NumWords>
auto ToDecimalString(std::array<uint64_t, NumWords> words) -> std::string {
  if (IsZero(words, NumWords)) {
    return "0";
  }

  std::string result;
  while (!IsZero(words, NumWords)) {
    uint64_t digit = DivideByWord(words, NumWords, 10);
    result.push_back(static_cast<char>('0' + digit));
  }
  std::reverse(result.begin(), result.end());
  return result;
}

}  // namespace detail

// WideBit<N> represents an N-bit value (N > 64) using multiple 64-bit words.
// Storage: std::array<uint64_t, WordsForBits(N)> with little-endian word order
// (LSB in words_[0])
//
// Design: Signedness is a template parameter WideBit<Width, Signed>.
// Rationale: Generated C++ code has static type information. Using template
// parameters enables compile-time optimization and matches Verilator's approach
// of generating type-specific functions.
// Compare to common::WideBit which passes signedness at runtime because the
// interpreter must handle dynamic types.
template <std::size_t Width, bool Signed = false>
class WideBit {
  static_assert(Width > 64, "Use Bit<N> for widths <= 64");
  static constexpr std::size_t kNumWords = detail::WordsForBits(Width);
  static constexpr uint64_t kFinalMask = detail::FinalWordMask(Width);

 public:
  static constexpr std::size_t kWidth = Width;
  static constexpr bool kSigned = Signed;

  // Default constructor - zero initialized
  constexpr WideBit() : words_{} {
  }

  // Construct from a single uint64_t value (in lowest word)
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr WideBit(uint64_t value) : words_{} {
    words_[0] = value;
  }

  // Construct from any integral type
  template <typename T>
    requires std::is_integral_v<T>
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr WideBit(T value) : words_{} {
    words_[0] = static_cast<uint64_t>(value);
    // Sign-extend if source is signed and negative
    if constexpr (std::is_signed_v<T>) {
      if (value < 0) {
        for (std::size_t i = 1; i < kNumWords; ++i) {
          words_[i] = ~0ULL;
        }
        MaskFinalWord();
      }
    }
  }

  // Construct from WideBit with same width but different signedness
  // Just copies the bits - signedness is a type-level distinction
  template <bool OtherSigned>
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr WideBit(const WideBit<Width, OtherSigned>& other) : words_{} {
    for (std::size_t i = 0; i < kNumWords; ++i) {
      words_[i] = other.GetWord(i);
    }
  }

  // Construct from WideBit with different width
  // Widening: copies all words, sign extends if source is signed and negative
  // Narrowing: truncates to fit (copies only the needed words)
  template <std::size_t OtherWidth, bool OtherSigned>
    requires(OtherWidth != Width)
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr WideBit(const WideBit<OtherWidth, OtherSigned>& other) : words_{} {
    constexpr std::size_t kOtherWords = detail::WordsForBits(OtherWidth);
    constexpr std::size_t kCopyWords =
        (kOtherWords < kNumWords) ? kOtherWords : kNumWords;

    // Copy common words
    for (std::size_t i = 0; i < kCopyWords; ++i) {
      words_[i] = other.GetWord(i);
    }

    // Handle widening with sign extension
    if constexpr (OtherWidth < Width && OtherSigned) {
      // Check if source is negative (sign bit set)
      constexpr std::size_t kSignBitWord = (OtherWidth - 1) / 64;
      constexpr std::size_t kSignBitPos = (OtherWidth - 1) % 64;
      bool is_negative = (other.GetWord(kSignBitWord) >> kSignBitPos) & 1;

      if (is_negative) {
        // Fill upper bits of the word containing the sign bit
        if constexpr (kSignBitPos < 63) {
          words_[kSignBitWord] |= (~0ULL << (kSignBitPos + 1));
        }
        // Fill remaining words with all 1s
        for (std::size_t i = kSignBitWord + 1; i < kNumWords; ++i) {
          words_[i] = ~0ULL;
        }
        MaskFinalWord();
      }
    }
  }

  // Construct from Bit<N> (N <= 64)
  // Sign-extends if source Bit is signed and negative
  template <std::size_t BitWidth, bool BitSigned>
    requires(BitWidth <= 64)
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr WideBit(const Bit<BitWidth, BitSigned>& bit) : words_{} {
    words_[0] = bit.Value();
    // Sign-extend if source is signed and sign bit is set
    if constexpr (BitSigned) {
      constexpr uint64_t kSignBit = 1ULL << (BitWidth - 1);
      if (bit.Value() & kSignBit) {
        // Fill low word's upper bits
        if constexpr (BitWidth < 64) {
          words_[0] |= ~((1ULL << BitWidth) - 1);
        }
        // Fill remaining words with all 1s
        for (std::size_t i = 1; i < kNumWords; ++i) {
          words_[i] = ~0ULL;
        }
        MaskFinalWord();
      }
    }
  }

  // Access to underlying words
  [[nodiscard]] constexpr auto GetWord(std::size_t index) const -> uint64_t {
    return words_[index];
  }

  constexpr auto SetWord(std::size_t index, uint64_t value) -> void {
    words_[index] = value;
  }

  // Value accessor (returns lowest 64 bits for compatibility)
  [[nodiscard]] constexpr auto Value() const -> uint64_t {
    return words_[0];
  }

  // Check if all bits are zero
  [[nodiscard]] constexpr auto IsZero() const -> bool {
    for (std::size_t i = 0; i < kNumWords; ++i) {
      if (words_[i] != 0) {
        return false;
      }
    }
    return true;
  }

  // Conversion to bool
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr operator bool() const {
    return !IsZero();
  }

  // Explicit conversion to integer types (truncates to low bits)
  template <typename T>
    requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
  explicit constexpr operator T() const {
    return static_cast<T>(words_[0]);
  }

  // Truncate to a narrower Bit<N> type.
  // Use this instead of implicit conversion for explicit truncation intent.
  // ResultSigned defaults to this WideBit's signedness.
  template <std::size_t ResultWidth, bool ResultSigned = Signed>
    requires(ResultWidth <= 64)
  [[nodiscard]] constexpr auto TruncateTo() const
      -> Bit<ResultWidth, ResultSigned> {
    return Bit<ResultWidth, ResultSigned>{*this};
  }

  // Get the sign bit
  [[nodiscard]] constexpr auto GetSignBit() const -> bool {
    std::size_t bit_in_final = (Width - 1) % 64;
    return (words_[kNumWords - 1] >> bit_in_final) & 1;
  }

  // Convert to signed type, preserving width and bit pattern
  [[nodiscard]] constexpr auto ToSigned() const -> WideBit<Width, true> {
    return WideBit<Width, true>{*this};
  }

  // Convert to unsigned type, preserving width and bit pattern
  [[nodiscard]] constexpr auto ToUnsigned() const -> WideBit<Width, false> {
    return WideBit<Width, false>{*this};
  }

  // Bitwise NOT
  [[nodiscard]] constexpr auto operator~() const -> WideBit {
    WideBit result;
    for (std::size_t i = 0; i < kNumWords; ++i) {
      result.words_[i] = ~words_[i];
    }
    result.MaskFinalWord();
    return result;
  }

  // Logical NOT
  [[nodiscard]] constexpr auto operator!() const -> bool {
    return IsZero();
  }

  // Unary plus (no-op)
  [[nodiscard]] constexpr auto operator+() const -> WideBit {
    return *this;
  }

  // Unary minus (two's complement)
  [[nodiscard]] constexpr auto operator-() const -> WideBit {
    WideBit result = ~(*this);
    WideBit one;
    one.words_[0] = 1;
    return result + one;
  }

  // Bitwise AND
  [[nodiscard]] constexpr auto operator&(const WideBit& other) const
      -> WideBit {
    WideBit result;
    for (std::size_t i = 0; i < kNumWords; ++i) {
      result.words_[i] = words_[i] & other.words_[i];
    }
    return result;
  }

  // Bitwise OR
  [[nodiscard]] constexpr auto operator|(const WideBit& other) const
      -> WideBit {
    WideBit result;
    for (std::size_t i = 0; i < kNumWords; ++i) {
      result.words_[i] = words_[i] | other.words_[i];
    }
    return result;
  }

  // Bitwise XOR
  [[nodiscard]] constexpr auto operator^(const WideBit& other) const
      -> WideBit {
    WideBit result;
    for (std::size_t i = 0; i < kNumWords; ++i) {
      result.words_[i] = words_[i] ^ other.words_[i];
    }
    return result;
  }

  // Addition with carry propagation
  [[nodiscard]] constexpr auto operator+(const WideBit& other) const
      -> WideBit {
    WideBit result;
    uint64_t carry = 0;
    for (std::size_t i = 0; i < kNumWords; ++i) {
      uint64_t a = words_[i];
      uint64_t b = other.words_[i];
      uint64_t sum = a + b;
      uint64_t carry1 = (sum < a) ? 1 : 0;
      uint64_t sum2 = sum + carry;
      uint64_t carry2 = (sum2 < sum) ? 1 : 0;
      result.words_[i] = sum2;
      carry = carry1 | carry2;
    }
    result.MaskFinalWord();
    return result;
  }

  // Subtraction
  [[nodiscard]] constexpr auto operator-(const WideBit& other) const
      -> WideBit {
    return *this + (-other);
  }

  // Multiplication (simple, may overflow - masked to width)
  [[nodiscard]] constexpr auto operator*(const WideBit& other) const
      -> WideBit {
    WideBit result;
    // School multiplication algorithm using __uint128_t for 64x64->128
    for (std::size_t i = 0; i < kNumWords; ++i) {
      if (other.words_[i] == 0) {
        continue;
      }
      uint64_t carry = 0;
      for (std::size_t j = 0; j + i < kNumWords; ++j) {
        // Use __uint128_t for accurate 64x64 -> 128-bit multiplication
        __uint128_t product =
            static_cast<__uint128_t>(words_[j]) * other.words_[i];
        __uint128_t sum = product + result.words_[i + j] + carry;
        result.words_[i + j] = static_cast<uint64_t>(sum);
        carry = static_cast<uint64_t>(sum >> 64);
      }
    }
    result.MaskFinalWord();
    return result;
  }

  // Equality
  [[nodiscard]] constexpr auto operator==(const WideBit& other) const -> bool {
    for (std::size_t i = 0; i < kNumWords; ++i) {
      if (words_[i] != other.words_[i]) {
        return false;
      }
    }
    return true;
  }

  [[nodiscard]] constexpr auto operator!=(const WideBit& other) const -> bool {
    return !(*this == other);
  }

  // Comparison (uses signed comparison if Signed template param is true)
  [[nodiscard]] constexpr auto operator<(const WideBit& other) const -> bool {
    if constexpr (Signed) {
      bool lhs_neg = GetSignBit();
      bool rhs_neg = other.GetSignBit();
      if (lhs_neg != rhs_neg) {
        return lhs_neg;  // negative < positive
      }
    }
    // Unsigned comparison from MSB to LSB
    for (std::size_t i = kNumWords; i > 0; --i) {
      if (words_[i - 1] < other.words_[i - 1]) {
        return true;
      }
      if (words_[i - 1] > other.words_[i - 1]) {
        return false;
      }
    }
    return false;  // equal
  }

  [[nodiscard]] constexpr auto operator<=(const WideBit& other) const -> bool {
    return !(other < *this);
  }

  [[nodiscard]] constexpr auto operator>(const WideBit& other) const -> bool {
    return other < *this;
  }

  [[nodiscard]] constexpr auto operator>=(const WideBit& other) const -> bool {
    return !(*this < other);
  }

  // Left shift
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator<<(T amount) const -> WideBit {
    if (amount <= 0) {
      return *this;
    }
    if (static_cast<std::size_t>(amount) >= Width) {
      return WideBit{};
    }

    WideBit result;
    std::size_t word_shift = static_cast<std::size_t>(amount) / 64;
    std::size_t bit_shift = static_cast<std::size_t>(amount) % 64;

    if (bit_shift == 0) {
      for (std::size_t i = word_shift; i < kNumWords; ++i) {
        result.words_[i] = words_[i - word_shift];
      }
    } else {
      for (std::size_t i = word_shift; i < kNumWords; ++i) {
        result.words_[i] = words_[i - word_shift] << bit_shift;
        if (i > word_shift) {
          result.words_[i] |= words_[i - word_shift - 1] >> (64 - bit_shift);
        }
      }
    }
    result.MaskFinalWord();
    return result;
  }

  // Right shift (logical for unsigned, arithmetic for signed)
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator>>(T amount) const -> WideBit {
    if (amount <= 0) {
      return *this;
    }

    bool sign_bit = Signed && GetSignBit();
    auto shift = static_cast<std::size_t>(amount);

    if (shift >= Width) {
      if (sign_bit) {
        WideBit result;
        for (std::size_t i = 0; i < kNumWords; ++i) {
          result.words_[i] = ~0ULL;
        }
        result.MaskFinalWord();
        return result;
      }
      return WideBit{};
    }

    WideBit result;
    std::size_t word_shift = shift / 64;
    std::size_t bit_shift = shift % 64;

    if (bit_shift == 0) {
      for (std::size_t i = 0; i + word_shift < kNumWords; ++i) {
        result.words_[i] = words_[i + word_shift];
      }
    } else {
      for (std::size_t i = 0; i + word_shift < kNumWords; ++i) {
        result.words_[i] = words_[i + word_shift] >> bit_shift;
        if (i + word_shift + 1 < kNumWords) {
          result.words_[i] |= words_[i + word_shift + 1] << (64 - bit_shift);
        }
      }
    }

    // Sign extend if needed - O(words) instead of O(bits)
    if (sign_bit) {
      std::size_t fill_start = Width - shift;
      std::size_t start_word = fill_start / 64;
      std::size_t start_bit = fill_start % 64;

      // Set upper bits of partial word
      if (start_bit != 0) {
        result.words_[start_word] |= (~0ULL << start_bit);
        ++start_word;
      }

      // Set remaining full words to all 1s
      for (std::size_t i = start_word; i < kNumWords; ++i) {
        result.words_[i] = ~0ULL;
      }
      result.MaskFinalWord();
    }

    return result;
  }

  // Shift operators with WideBit as shift amount
  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto operator<<(const WideBit<W, S>& amount) const
      -> WideBit {
    return *this << static_cast<std::size_t>(amount.Value());
  }

  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto operator>>(const WideBit<W, S>& amount) const
      -> WideBit {
    return *this >> static_cast<std::size_t>(amount.Value());
  }

  // Shift operators with Bit as shift amount
  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto operator<<(const Bit<W, S>& amount) const
      -> WideBit {
    return *this << static_cast<std::size_t>(amount.Value());
  }

  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto operator>>(const Bit<W, S>& amount) const
      -> WideBit {
    return *this >> static_cast<std::size_t>(amount.Value());
  }

  // Get single bit (returns Bit<1, false> for consistency with Bit::GetBit)
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto GetBit(T index) const -> Bit<1, false> {
    auto idx = static_cast<std::size_t>(index);
    std::size_t word_idx = idx / 64;
    std::size_t bit_idx = idx % 64;
    if (word_idx >= kNumWords) {
      return Bit<1, false>{0};
    }
    return Bit<1, false>{
        static_cast<uint8_t>((words_[word_idx] >> bit_idx) & 1)};
  }

  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto GetBit(const Bit<W, S>& index) const
      -> Bit<1, false> {
    return GetBit(static_cast<std::size_t>(index.Value()));
  }

  // Extract a slice [start_bit, start_bit + slice_width) as a new WideBit.
  // Used for multi-dimensional packed array element access.
  // Returns WideBit with same Width (caller should cast to narrower type).
  // For a properly-typed result, use ExtractSliceTo<ResultWidth>().
  template <typename T1, typename T2>
    requires std::is_integral_v<T1> && std::is_integral_v<T2>
  [[nodiscard]] constexpr auto ExtractSlice(T1 start_bit, T2 slice_width) const
      -> WideBit {
    // Shift right to align slice to bit 0, then mask
    auto start = static_cast<std::size_t>(start_bit);
    auto width = static_cast<std::size_t>(slice_width);

    WideBit shifted = *this >> start;
    // Mask to slice_width bits
    if (width < 64) {
      shifted.words_[0] &= (1ULL << width) - 1;
      for (std::size_t i = 1; i < kNumWords; ++i) {
        shifted.words_[i] = 0;
      }
    } else {
      std::size_t full_words = width / 64;
      std::size_t remaining_bits = width % 64;
      if (remaining_bits > 0 && full_words < kNumWords) {
        shifted.words_[full_words] &= (1ULL << remaining_bits) - 1;
      }
      for (std::size_t i = full_words + (remaining_bits > 0 ? 1 : 0);
           i < kNumWords; ++i) {
        shifted.words_[i] = 0;
      }
    }
    return shifted;
  }

  // Extract a slice and return it as a specific result type.
  // Use this when you know the result width at compile time.
  // ResultType can be Bit<N> (for slices <= 64 bits) or WideBit<N> (for wider).
  template <typename ResultType, typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto ExtractSliceTo(T start_bit) const -> ResultType {
    auto start = static_cast<std::size_t>(start_bit);
    WideBit shifted = *this >> start;
    return ResultType{shifted};
  }

  // Insert a value at a bit position: result[start_bit +: element_width] =
  // value Returns new WideBit with value inserted (immutable operation). Used
  // for multi-dimensional packed array element assignment. Accepts any WideBit
  // type as value (will be expanded/truncated to fit).
  template <std::size_t ValueWidth, bool ValueSigned, typename T1, typename T2>
    requires std::is_integral_v<T1> && std::is_integral_v<T2>
  [[nodiscard]] constexpr auto InsertSlice(
      const WideBit<ValueWidth, ValueSigned>& value, T1 start_bit,
      T2 element_width) const -> WideBit {
    auto start = static_cast<std::size_t>(start_bit);
    auto width = static_cast<std::size_t>(element_width);

    // Create mask of element_width bits
    WideBit mask;
    if (width >= Width) {
      for (std::size_t i = 0; i < kNumWords; ++i) {
        mask.words_[i] = ~0ULL;
      }
      mask.MaskFinalWord();
    } else if (width < 64) {
      mask.words_[0] = (1ULL << width) - 1;
    } else {
      std::size_t full_words = width / 64;
      std::size_t remaining_bits = width % 64;
      for (std::size_t i = 0; i < full_words; ++i) {
        mask.words_[i] = ~0ULL;
      }
      if (remaining_bits > 0 && full_words < kNumWords) {
        mask.words_[full_words] = (1ULL << remaining_bits) - 1;
      }
    }

    // Copy value words into storage-sized WideBit, then mask
    WideBit expanded_value;
    constexpr std::size_t kValueWords = detail::WordsForBits(ValueWidth);
    for (std::size_t i = 0; i < kNumWords && i < kValueWords; ++i) {
      expanded_value.words_[i] = value.GetWord(i);
    }
    WideBit masked_value = expanded_value & mask;

    // Shift mask and value to position
    WideBit shifted_mask = mask << start;
    WideBit shifted_value = masked_value << start;

    // Clear slot and insert: (this & ~shifted_mask) | shifted_value
    return (*this & ~shifted_mask) | shifted_value;
  }

  // InsertSlice overload for Bit<N> values (required for template deduction)
  // Implicit conversion doesn't work in template argument deduction context,
  // so we need this explicit overload to dispatch to the WideBit version.
  template <std::size_t ValueWidth, bool ValueSigned, typename T1, typename T2>
    requires std::is_integral_v<T1> && std::is_integral_v<T2> &&
             (ValueWidth <= 64)
  [[nodiscard]] constexpr auto InsertSlice(
      const Bit<ValueWidth, ValueSigned>& value, T1 start_bit,
      T2 element_width) const -> WideBit {
    // Construct WideBit from Bit (uses constructor with proper sign extension)
    return InsertSlice(
        WideBit<Width, ValueSigned>{value}, start_bit, element_width);
  }

  // Compound assignment
  constexpr auto operator&=(const WideBit& other) -> WideBit& {
    for (std::size_t i = 0; i < kNumWords; ++i) {
      words_[i] &= other.words_[i];
    }
    return *this;
  }

  constexpr auto operator|=(const WideBit& other) -> WideBit& {
    for (std::size_t i = 0; i < kNumWords; ++i) {
      words_[i] |= other.words_[i];
    }
    return *this;
  }

  constexpr auto operator^=(const WideBit& other) -> WideBit& {
    for (std::size_t i = 0; i < kNumWords; ++i) {
      words_[i] ^= other.words_[i];
    }
    return *this;
  }

  constexpr auto operator+=(const WideBit& other) -> WideBit& {
    *this = *this + other;
    return *this;
  }

  constexpr auto operator-=(const WideBit& other) -> WideBit& {
    *this = *this - other;
    return *this;
  }

  // Pre/post increment/decrement
  constexpr auto operator++() -> WideBit& {
    WideBit one;
    one.words_[0] = 1;
    *this = *this + one;
    return *this;
  }

  constexpr auto operator++(int) -> WideBit {
    WideBit tmp = *this;
    ++(*this);
    return tmp;
  }

  constexpr auto operator--() -> WideBit& {
    WideBit one;
    one.words_[0] = 1;
    *this = *this - one;
    return *this;
  }

  constexpr auto operator--(int) -> WideBit {
    WideBit tmp = *this;
    --(*this);
    return tmp;
  }

  // Stream output (hex format)
  friend auto operator<<(std::ostream& os, const WideBit& b) -> std::ostream& {
    os << "0x";
    bool leading = true;
    for (std::size_t i = kNumWords; i > 0; --i) {
      uint64_t word = b.words_[i - 1];
      if (leading && word == 0 && i > 1) {
        continue;
      }
      if (leading) {
        os << std::format("{:x}", word);
        leading = false;
      } else {
        os << std::format("{:016x}", word);
      }
    }
    if (leading) {
      os << "0";
    }
    return os;
  }

 private:
  std::array<uint64_t, kNumWords> words_;

  constexpr auto MaskFinalWord() -> void {
    words_[kNumWords - 1] &= kFinalMask;
  }
};

// Logical AND for WideBit types
template <std::size_t W1, bool S1, std::size_t W2, bool S2>
[[nodiscard]] constexpr auto operator&&(
    const WideBit<W1, S1>& lhs, const WideBit<W2, S2>& rhs) -> bool {
  return static_cast<bool>(lhs) && static_cast<bool>(rhs);
}

// Logical OR for WideBit types
template <std::size_t W1, bool S1, std::size_t W2, bool S2>
[[nodiscard]] constexpr auto operator||(
    const WideBit<W1, S1>& lhs, const WideBit<W2, S2>& rhs) -> bool {
  return static_cast<bool>(lhs) || static_cast<bool>(rhs);
}

namespace detail {

// Helper to get bit width from any operand type
template <typename T>
constexpr auto GetBitWidth() -> std::size_t {
  if constexpr (requires { T::kWidth; }) {
    return T::kWidth;  // WideBit or Bit with static kWidth member
  } else if constexpr (std::is_integral_v<T>) {
    return sizeof(T) * 8;
  } else {
    return 0;
  }
}

// Helper to get value from operand, masked to its actual width.
// This handles sign-extension correctly - a signed int8_t with value -1
// returns 0xFF, not 0xFFFFFFFFFFFFFFFF.
template <typename T>
constexpr auto GetMaskedValue(const T& operand) -> uint64_t {
  constexpr std::size_t kWidth = GetBitWidth<T>();
  uint64_t value = 0;
  if constexpr (requires { operand.Value(); }) {
    value = operand.Value();
  } else if constexpr (std::is_integral_v<T>) {
    value = static_cast<uint64_t>(operand);
  }
  // Mask to operand width to handle sign-extension
  if constexpr (kWidth < 64) {
    value &= (1ULL << kWidth) - 1;
  }
  return value;
}

// Helper to insert bits from any operand type into WideBit at a position
template <std::size_t ResultWidth, typename T>
constexpr void InsertBits(
    WideBit<ResultWidth>& result, const T& operand, std::size_t bit_pos) {
  constexpr std::size_t kResultWords = WordsForBits(ResultWidth);
  constexpr std::size_t kOperandWidth = GetBitWidth<T>();

  // For narrow operands (<= 64 bits), just insert the single value
  if constexpr (kOperandWidth <= 64) {
    // Use GetMaskedValue to handle sign-extension correctly
    uint64_t value = GetMaskedValue(operand);
    std::size_t word_idx = bit_pos / 64;
    std::size_t bit_offset = bit_pos % 64;

    if (word_idx < kResultWords) {
      result.SetWord(
          word_idx, result.GetWord(word_idx) | (value << bit_offset));
      // Handle overflow into next word
      if (bit_offset > 0 && word_idx + 1 < kResultWords) {
        result.SetWord(
            word_idx + 1,
            result.GetWord(word_idx + 1) | (value >> (64 - bit_offset)));
      }
    }
  } else {
    // Wide operand - copy all words
    constexpr std::size_t kOperandWords = WordsForBits(kOperandWidth);
    std::size_t start_word = bit_pos / 64;
    std::size_t bit_offset = bit_pos % 64;

    for (std::size_t i = 0; i < kOperandWords && start_word + i < kResultWords;
         ++i) {
      uint64_t word = operand.GetWord(i);
      std::size_t dst_idx = start_word + i;

      if (bit_offset == 0) {
        result.SetWord(dst_idx, result.GetWord(dst_idx) | word);
      } else {
        result.SetWord(dst_idx, result.GetWord(dst_idx) | (word << bit_offset));
        if (dst_idx + 1 < kResultWords) {
          result.SetWord(
              dst_idx + 1,
              result.GetWord(dst_idx + 1) | (word >> (64 - bit_offset)));
        }
      }
    }
  }
}

// Recursive helper to process operands right-to-left (wide result)
template <std::size_t ResultWidth, typename First, typename... Rest>
constexpr void WideConcatHelper(
    WideBit<ResultWidth>& result, std::size_t bit_pos, const First& first,
    const Rest&... rest) {
  // Process remaining operands first (they go at lower bit positions)
  if constexpr (sizeof...(rest) > 0) {
    WideConcatHelper(result, bit_pos, rest...);
    // Update bit_pos for this operand
    std::size_t rest_width = (GetBitWidth<Rest>() + ...);
    InsertBits(result, first, bit_pos + rest_width);
  } else {
    // Last operand (rightmost, LSB position)
    InsertBits(result, first, bit_pos);
  }
}

// Narrow concatenation helper - base case (single operand)
template <typename First>
constexpr auto NarrowConcatHelper(const First& first) -> uint64_t {
  return GetMaskedValue(first);
}

// Narrow concatenation helper - recursive case
template <typename First, typename... Rest>
  requires(sizeof...(Rest) > 0)
constexpr auto NarrowConcatHelper(const First& first, const Rest&... rest)
    -> uint64_t {
  // Process rest first (they are at lower bit positions)
  uint64_t rest_result = NarrowConcatHelper(rest...);
  // Calculate shift for first operand (sum of remaining operand widths)
  constexpr std::size_t kRestWidth = (GetBitWidth<Rest>() + ...);
  // Get first value masked to its width
  uint64_t first_value = GetMaskedValue(first);
  return rest_result | (first_value << kRestWidth);
}

// Wrapper for narrow concatenation results that preserves width information.
// This is needed for nested concatenations: in {concat8, concat8}, the inner
// results need to report their 8-bit width, not uint64_t's 64-bit width.
template <std::size_t Width>
struct NarrowConcatResult {
  static_assert(Width > 0 && Width <= 64, "Width must be 1-64");
  static constexpr std::size_t kWidth = Width;

  uint64_t value;

  constexpr NarrowConcatResult() : value{0} {
  }
  explicit constexpr NarrowConcatResult(uint64_t v) : value{v} {
  }

  // Implicit conversion to integral types for assignment
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr operator uint64_t() const {
    return value;
  }

  // Value() method for GetMaskedValue
  [[nodiscard]] constexpr auto Value() const -> uint64_t {
    return value;
  }
};

}  // namespace detail

// Concatenation function - handles both narrow (<= 64 bit) and wide (> 64 bit)
// results. Usage: Concat<96>(a, b, c) where a is MSB.
// Operands can be WideBit, Bit, or integral types.
// Returns NarrowConcatResult<N> for narrow, WideBit<N> for wide results.
// NarrowConcatResult carries width info for correct nested concatenation.
template <std::size_t ResultWidth, typename... Args>
[[nodiscard]] constexpr auto Concat(const Args&... args) {
  if constexpr (ResultWidth <= 64) {
    // Narrow path: return NarrowConcatResult to preserve width information
    if constexpr (sizeof...(args) == 0) {
      return detail::NarrowConcatResult<ResultWidth>{0};
    } else {
      return detail::NarrowConcatResult<ResultWidth>{
          detail::NarrowConcatHelper(args...)};
    }
  } else {
    // Wide path: return WideBit<ResultWidth>
    WideBit<ResultWidth> result;
    if constexpr (sizeof...(args) > 0) {
      detail::WideConcatHelper(result, 0, args...);
    }
    return result;
  }
}

namespace detail {

// Helper for Replicate - expands value into Count copies and calls Concat
template <std::size_t ResultWidth, typename T, std::size_t... Is>
[[nodiscard]] constexpr auto ReplicateImpl(
    const T& value, std::index_sequence<Is...> /*unused*/) {
  // (void(Is), value)... expands to (value, value, value, ...) Count times
  return Concat<ResultWidth>((static_cast<void>(Is), value)...);
}

}  // namespace detail

// Replication function - replicates a value Count times.
// Usage: Replicate<32, 4>(byte_val) replicates an 8-bit value 4 times.
// Equivalent to Concat<32>(val, val, val, val) but more concise.
template <std::size_t ResultWidth, std::size_t Count, typename T>
[[nodiscard]] constexpr auto Replicate(const T& value) {
  return detail::ReplicateImpl<ResultWidth>(
      value, std::make_index_sequence<Count>{});
}

}  // namespace lyra::sdk

// Include Bit after WideBit is fully defined to resolve circular dependency.
// WideBit::GetBit() returns Bit<1, false> which needs the full Bit definition.
#include "lyra/sdk/bit.hpp"

// std::formatter specialization for WideBit<N, S>
template <std::size_t Width, bool Signed>
struct std::formatter<lyra::sdk::WideBit<Width, Signed>> {
  char spec = 'd';

  constexpr auto parse(std::format_parse_context& ctx)
      -> std::format_parse_context::iterator {
    const char* it = ctx.begin();
    if (it != ctx.end()) {
      if (*it == 'd' || *it == 'x') {
        spec = *it;
        return std::next(it);
      }
    }
    return it;
  }

  auto format(
      const lyra::sdk::WideBit<Width, Signed>& b,
      std::format_context& ctx) const {
    constexpr std::size_t kNumWords = lyra::sdk::detail::WordsForBits(Width);

    // Decimal format: use division-based conversion
    if (spec == 'd') {
      std::array<uint64_t, kNumWords> words{};
      for (std::size_t i = 0; i < kNumWords; ++i) {
        words[i] = b.GetWord(i);
      }
      return std::format_to(
          ctx.out(), "{}", lyra::sdk::detail::ToDecimalString(words));
    }

    // Hex format (default for 'x' or unrecognized specs)
    std::string result = "0x";
    bool leading = true;
    for (std::size_t i = kNumWords; i > 0; --i) {
      uint64_t word = b.GetWord(i - 1);
      if (leading && word == 0 && i > 1) {
        continue;
      }
      if (leading) {
        result += std::format("{:x}", word);
        leading = false;
      } else {
        result += std::format("{:016x}", word);
      }
    }
    if (leading) {
      result += "0";
    }
    return std::format_to(ctx.out(), "{}", result);
  }
};
