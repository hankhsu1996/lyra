#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/fixed_array.hpp"

namespace lyra::value {

inline constexpr std::size_t kPackedWordsInlineCapacity = 1;

using PackedWordArray =
    lyra::base::FixedArray<std::uint64_t, kPackedWordsInlineCapacity>;

enum class Signedness : std::uint8_t { kSigned, kUnsigned };
enum class TwoStateBit : std::uint8_t { kZero, kOne };
enum class FourStateBit : std::uint8_t {
  kZero,
  kOne,
  kHighImpedance,
  kUnknown
};

[[nodiscard]] constexpr auto WordCountForBits(std::uint64_t bit_width)
    -> std::size_t {
  // 0 bits is the `PackedArray()` sentinel "uninitialized" shape; an empty
  // storage matches it. Non-zero widths round up to whole words.
  // Overflow-safe form: never compute `bit_width + 63`.
  const std::uint64_t whole = bit_width / 64U;
  const std::uint64_t remainder = bit_width % 64U;
  return static_cast<std::size_t>(whole + (remainder == 0U ? 0U : 1U));
}

// The bits of word `word_index` that lie below `bit_width`: every bit of a word
// the width reaches past, no bit of a word beyond it, and a partial mask in the
// one word the width ends inside.
[[nodiscard]] constexpr auto ValidBitsMask(
    std::size_t word_index, std::uint64_t bit_width) -> std::uint64_t {
  const std::uint64_t low = static_cast<std::uint64_t>(word_index) * 64U;
  const std::uint64_t within = bit_width > low ? bit_width - low : 0U;
  if (within >= 64U) {
    return ~std::uint64_t{0};
  }
  return (std::uint64_t{1} << within) - 1U;
}

// A position past the end reads as clear, which is what a plane the value does
// not carry answers for every one of its bits.
[[nodiscard]] constexpr auto BitAt(
    std::span<const std::uint64_t> words, std::uint64_t position) -> bool {
  const auto index = static_cast<std::size_t>(position / 64U);
  if (index >= words.size()) {
    return false;
  }
  return ((words[index] >> (position % 64U)) & 1U) != 0U;
}

auto MaskUnusedTopBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void;

// Every position below `bit_width` set, and nothing above it.
auto SetAllValidBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void;

// The word planes of one packed value, borrowed. A plane holds exactly as many
// words as its width needs, with bit i of the value at bit i%64 of word i/64,
// so a reader takes the words and the width together and establishes nothing
// about either. A four-state value's second plane marks the positions holding x
// or z; a two-state value has none, and the empty plane reads as clear
// everywhere.
class ConstBitView {
 public:
  ConstBitView(std::span<const std::uint64_t> words, std::uint64_t bit_width);

  [[nodiscard]] auto ValueWords() const -> std::span<const std::uint64_t>;
  [[nodiscard]] auto Width() const -> std::uint64_t;

 private:
  std::span<const std::uint64_t> words_;
  std::uint64_t bit_width_;
};

class BitView {
 public:
  BitView(std::span<std::uint64_t> words, std::uint64_t bit_width);

  [[nodiscard]] auto ValueWords() const -> std::span<std::uint64_t>;
  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto AsConst() const -> ConstBitView;

 private:
  std::span<std::uint64_t> words_;
  std::uint64_t bit_width_;
};

class ConstLogicView {
 public:
  ConstLogicView(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words, std::uint64_t bit_width);

  [[nodiscard]] auto ValueWords() const -> std::span<const std::uint64_t>;
  [[nodiscard]] auto UnknownWords() const -> std::span<const std::uint64_t>;
  [[nodiscard]] auto Width() const -> std::uint64_t;

 private:
  std::span<const std::uint64_t> value_words_;
  std::span<const std::uint64_t> unknown_words_;
  std::uint64_t bit_width_;
};

class LogicView {
 public:
  LogicView(
      std::span<std::uint64_t> value_words,
      std::span<std::uint64_t> unknown_words, std::uint64_t bit_width);

  [[nodiscard]] auto ValueWords() const -> std::span<std::uint64_t>;
  [[nodiscard]] auto UnknownWords() const -> std::span<std::uint64_t>;
  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto AsConst() const -> ConstLogicView;

 private:
  std::span<std::uint64_t> value_words_;
  std::span<std::uint64_t> unknown_words_;
  std::uint64_t bit_width_;
};

}  // namespace lyra::value
