#pragma once

#include <cstddef>
#include <cstdint>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/wide_bit_ops.hpp"

namespace lyra::common {

// WideBit represents an arbitrary-width bit vector (>64 bits).
// Storage: std::vector<uint64_t> with little-endian word order (LSB in
// words_[0]) Note: Logical width is tracked in Type, not here - the class is
// width-agnostic for most operations, with width passed to operations that need
// masking.
//
// Design: Signedness passed as runtime parameter (vs SDK's template parameter).
// Rationale: The interpreter handles type information at runtime - the same
// WideBit object may be interpreted as signed or unsigned depending on the
// operation context. This matches how RuntimeValue stores signedness in Type.
// Compare to sdk::WideBit<N, Signed> which uses template parameters because
// codegen determines types at compile time.
class WideBit {
 public:
  static constexpr size_t kBitsPerWord = 64;

  // Default constructor - creates empty WideBit
  WideBit() = default;

  // Construct with specified number of words (all zero-initialized)
  explicit WideBit(size_t num_words) : words_(num_words, 0) {
  }

  // Construct from raw word data
  explicit WideBit(std::vector<uint64_t> words) : words_(std::move(words)) {
  }

  // Construct from a single uint64_t value
  static auto FromUInt64(uint64_t value, size_t num_words) -> WideBit {
    WideBit result(num_words);
    if (num_words > 0) {
      result.words_[0] = value;
    }
    return result;
  }

  // Construct from bit width (convenience)
  static auto FromBitWidth(size_t bit_width) -> WideBit {
    return WideBit(wide_ops::WordsForBits(bit_width));
  }

  // Access
  [[nodiscard]] auto NumWords() const -> size_t {
    return words_.size();
  }

  [[nodiscard]] auto GetWord(size_t index) const -> uint64_t {
    if (index >= words_.size()) {
      throw InternalError(
          "WideBit::GetWord",
          fmt::format(
              "index {} out of bounds (size {})", index, words_.size()));
    }
    return words_[index];
  }

  auto SetWord(size_t index, uint64_t value) -> void {
    if (index >= words_.size()) {
      throw InternalError(
          "WideBit::SetWord",
          fmt::format(
              "index {} out of bounds (size {})", index, words_.size()));
    }
    words_[index] = value;
  }

  [[nodiscard]] auto Words() const -> const std::vector<uint64_t>& {
    return words_;
  }

  [[nodiscard]] auto Words() -> std::vector<uint64_t>& {
    return words_;
  }

  // Mask the value to a specific bit width (modifies in place)
  auto MaskToWidth(size_t bit_width) -> void {
    wide_ops::MaskToWidth(words_, bit_width);
  }

  // Create a masked copy
  [[nodiscard]] auto MaskedToWidth(size_t bit_width) const -> WideBit {
    WideBit result = *this;
    result.MaskToWidth(bit_width);
    return result;
  }

  // Bitwise NOT (requires bit_width to mask upper bits)
  [[nodiscard]] auto BitwiseNot(size_t bit_width) const -> WideBit {
    WideBit result(words_.size());
    wide_ops::BitwiseNot(words_, result.words_, bit_width);
    return result;
  }

  // Bitwise AND
  [[nodiscard]] auto operator&(const WideBit& other) const -> WideBit {
    CheckSameSize("operator&", other);
    WideBit result(words_.size());
    wide_ops::BitwiseAnd(words_, other.words_, result.words_);
    return result;
  }

  // Bitwise OR
  [[nodiscard]] auto operator|(const WideBit& other) const -> WideBit {
    CheckSameSize("operator|", other);
    WideBit result(words_.size());
    wide_ops::BitwiseOr(words_, other.words_, result.words_);
    return result;
  }

  // Bitwise XOR
  [[nodiscard]] auto operator^(const WideBit& other) const -> WideBit {
    CheckSameSize("operator^", other);
    WideBit result(words_.size());
    wide_ops::BitwiseXor(words_, other.words_, result.words_);
    return result;
  }

  // Addition with carry propagation
  [[nodiscard]] auto Add(const WideBit& other, size_t bit_width) const
      -> WideBit {
    CheckSameSize("Add", other);
    WideBit result(words_.size());
    wide_ops::Add(words_, other.words_, result.words_, bit_width);
    return result;
  }

  // Subtraction via two's complement
  [[nodiscard]] auto Sub(const WideBit& other, size_t bit_width) const
      -> WideBit {
    // Compute two's complement: -other = ~other + 1
    WideBit negated = other.BitwiseNot(bit_width);

    WideBit one(words_.size());
    one.words_[0] = 1;

    WideBit neg_other = negated.Add(one, bit_width);
    return Add(neg_other, bit_width);
  }

  // Negation via two's complement: -x = ~x + 1
  [[nodiscard]] auto Negate(size_t bit_width) const -> WideBit {
    WideBit negated = BitwiseNot(bit_width);

    WideBit one(words_.size());
    one.words_[0] = 1;

    return negated.Add(one, bit_width);
  }

  // Multiplication with carry propagation
  [[nodiscard]] auto Mul(const WideBit& other, size_t bit_width) const
      -> WideBit {
    CheckSameSize("Mul", other);
    WideBit result(words_.size());
    wide_ops::Multiply(words_, other.words_, result.words_, bit_width);
    return result;
  }

  // Left shift
  [[nodiscard]] auto ShiftLeft(size_t amount, size_t bit_width) const
      -> WideBit {
    WideBit result(words_.size());
    wide_ops::ShiftLeft(words_, result.words_, amount, bit_width);
    return result;
  }

  // Logical right shift (zero-fill)
  [[nodiscard]] auto ShiftRightLogical(size_t amount) const -> WideBit {
    WideBit result(words_.size());
    wide_ops::ShiftRightLogical(words_, result.words_, amount);
    return result;
  }

  // Arithmetic right shift (sign-extend)
  [[nodiscard]] auto ShiftRightArithmetic(size_t amount, size_t bit_width) const
      -> WideBit {
    WideBit result(words_.size());
    wide_ops::ShiftRightArithmetic(words_, result.words_, amount, bit_width);
    return result;
  }

  // Equality comparison
  [[nodiscard]] auto operator==(const WideBit& other) const -> bool {
    return wide_ops::Equal(words_, other.words_);
  }

  [[nodiscard]] auto operator!=(const WideBit& other) const -> bool {
    return !(*this == other);
  }

  // Unsigned less-than comparison (compares as unsigned integers)
  [[nodiscard]] auto LessThanUnsigned(const WideBit& other) const -> bool {
    CheckSameSize("LessThanUnsigned", other);
    // Compare from MSB to LSB
    for (auto i = static_cast<int>(words_.size()) - 1; i >= 0; --i) {
      if (words_[i] < other.words_[i]) {
        return true;
      }
      if (words_[i] > other.words_[i]) {
        return false;
      }
    }
    return false;  // Equal
  }

  // Signed less-than comparison (interprets as two's complement)
  // Requires bit_width to determine sign bit position
  [[nodiscard]] auto LessThanSigned(
      const WideBit& other, size_t bit_width) const -> bool {
    CheckSameSize("LessThanSigned", other);

    // Get sign bits (MSB of the logical value)
    bool this_negative = GetBit(bit_width - 1) != 0;
    bool other_negative = other.GetBit(bit_width - 1) != 0;

    // If signs differ, negative is less than positive
    if (this_negative != other_negative) {
      return this_negative;  // negative < positive
    }

    // Same sign: use unsigned comparison
    // For same sign, unsigned comparison gives correct result
    return LessThanUnsigned(other);
  }

  // Generic less-than that takes signedness as parameter
  [[nodiscard]] auto LessThan(
      const WideBit& other, size_t bit_width, bool is_signed) const -> bool {
    if (is_signed) {
      return LessThanSigned(other, bit_width);
    }
    return LessThanUnsigned(other);
  }

  // Check if all bits are zero
  [[nodiscard]] auto IsZero() const -> bool {
    return wide_ops::IsZero(words_);
  }

  // Conversion to bool (any bit set?)
  [[nodiscard]] explicit operator bool() const {
    return !IsZero();
  }

  // Get a single bit (returns 0 or 1)
  [[nodiscard]] auto GetBit(size_t index) const -> uint64_t {
    return wide_ops::GetBit(words_, index);
  }

  // Set a single bit
  auto SetBit(size_t index, bool value) -> void {
    wide_ops::SetBit(words_, index, value);
  }

  // String representation (hex format, MSB first)
  [[nodiscard]] auto ToHexString() const -> std::string {
    return wide_ops::ToHexStringImpl(words_, [](uint64_t word, bool padded) {
      return padded ? fmt::format("{:016x}", word) : fmt::format("{:x}", word);
    });
  }

  // String representation (binary format, MSB first)
  [[nodiscard]] auto ToBinaryString() const -> std::string {
    return wide_ops::ToBinaryString(words_);
  }

  // String representation (octal format, MSB first)
  [[nodiscard]] auto ToOctalString() const -> std::string {
    return wide_ops::ToOctalStringImpl(words_, [](uint64_t word, bool padded) {
      return padded ? fmt::format("{:022o}", word) : fmt::format("{:o}", word);
    });
  }

  // String representation (decimal format, unsigned)
  [[nodiscard]] auto ToDecimalString() const -> std::string {
    return wide_ops::ToDecimalStringImpl(words_);
  }

  // String representation (decimal format, signed)
  // Requires bit_width to determine sign bit position
  [[nodiscard]] auto ToDecimalStringSigned(size_t bit_width) const
      -> std::string {
    bool negative = GetBit(bit_width - 1) != 0;
    if (!negative) {
      return ToDecimalString();
    }
    // Two's complement: negate then format with '-' prefix
    WideBit negated = Negate(bit_width);
    return "-" + negated.ToDecimalString();
  }

  // Convert to double (unsigned interpretation)
  // For signed conversion, caller should handle two's complement
  [[nodiscard]] auto ToDouble() const -> double {
    // Build value from MSB to LSB: result = result * 2^64 + word[i]
    constexpr double kTwoTo64 = 18446744073709551616.0;  // 2^64
    double result = 0.0;
    for (auto i = static_cast<int>(words_.size()) - 1; i >= 0; --i) {
      result = result * kTwoTo64 + static_cast<double>(words_[i]);
    }
    return result;
  }

  // Hash for use in containers
  [[nodiscard]] auto Hash() const -> std::size_t {
    std::size_t h = 0;
    for (uint64_t word : words_) {
      h ^= std::hash<uint64_t>{}(word) + 0x9e3779b9 + (h << 6) + (h >> 2);
    }
    return h;
  }

  // Extract a slice [start_bit, start_bit + width) as a new WideBit.
  // Used for multi-dimensional packed array element access.
  [[nodiscard]] auto ExtractSlice(size_t start_bit, size_t width) const
      -> WideBit {
    // Shift right to align slice to bit 0, then mask to width
    auto shifted = ShiftRightLogical(start_bit);
    // Resize to minimum words needed for result width
    size_t result_words = wide_ops::WordsForBits(width);
    WideBit result(result_words);
    for (size_t i = 0; i < result_words && i < shifted.words_.size(); ++i) {
      result.words_[i] = shifted.words_[i];
    }
    result.MaskToWidth(width);
    return result;
  }

  // Insert a value at a bit position: result[start_bit +: width] = value.
  // Returns new WideBit with value inserted (immutable operation).
  // Used for multi-dimensional packed array element assignment.
  [[nodiscard]] auto InsertSlice(
      const WideBit& value, size_t start_bit, size_t element_width) const
      -> WideBit {
    // Create mask for the element slot
    WideBit mask = WideBit::FromBitWidth(words_.size() * kBitsPerWord);
    for (auto& word : mask.words_) {
      word = ~uint64_t{0};
    }
    mask.MaskToWidth(element_width);

    // Expand value to same size as this
    WideBit expanded_value(words_.size());
    for (size_t i = 0; i < value.words_.size() && i < words_.size(); ++i) {
      expanded_value.words_[i] = value.words_[i];
    }
    expanded_value.MaskToWidth(element_width);

    // Shift mask and value to position
    size_t total_width = words_.size() * kBitsPerWord;
    WideBit shifted_mask = mask.ShiftLeft(start_bit, total_width);
    WideBit shifted_value = expanded_value.ShiftLeft(start_bit, total_width);

    // Clear slot and insert: (this & ~shifted_mask) | shifted_value
    WideBit inverted_mask = shifted_mask.BitwiseNot(total_width);
    WideBit cleared = *this & inverted_mask;
    return cleared | shifted_value;
  }

 private:
  std::vector<uint64_t> words_;  // Little-endian: words_[0] is LSB

  // Helper to check operands have same size
  auto CheckSameSize(const char* op, const WideBit& other) const -> void {
    if (words_.size() != other.words_.size()) {
      throw InternalError(
          op, fmt::format(
                  "WideBit operand size mismatch: {} vs {}", words_.size(),
                  other.words_.size()));
    }
  }
};

// Stream output
inline auto operator<<(std::ostream& os, const WideBit& wb) -> std::ostream& {
  return os << wb.ToHexString();
}

}  // namespace lyra::common

// fmt::formatter support
template <>
struct fmt::formatter<lyra::common::WideBit> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::WideBit& wb, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", wb.ToHexString());
  }
};
