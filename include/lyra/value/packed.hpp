#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/inlined_vector.hpp"

namespace lyra::value {

inline constexpr std::size_t kPackedWordsInlineCapacity = 1;

using PackedWordVector =
    lyra::base::InlinedVector<std::uint64_t, kPackedWordsInlineCapacity>;

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
  // storage vector matches it. Non-zero widths round up to whole words.
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

class PackedWords {
 public:
  explicit PackedWords(std::uint64_t bit_width);

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;

  [[nodiscard]] auto Words() -> std::span<std::uint64_t>;
  [[nodiscard]] auto Words() const -> std::span<const std::uint64_t>;

  auto SetOne() -> void;

 private:
  std::uint64_t bit_width_;
  PackedWordVector words_;
};

class ConstBitView;
class BitView;
class ConstLogicView;
class LogicView;

namespace detail {
struct PackedAccess;
}  // namespace detail

class ConstBitView {
 public:
  ConstBitView(
      std::span<const std::uint64_t> words, std::uint64_t bit_offset,
      std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

 private:
  friend struct detail::PackedAccess;

  std::span<const std::uint64_t> words_;
  std::uint64_t bit_offset_;
  std::uint64_t bit_width_;
};

class BitView {
 public:
  BitView(
      std::span<std::uint64_t> words, std::uint64_t bit_offset,
      std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto AsConst() const -> ConstBitView;

 private:
  friend struct detail::PackedAccess;

  std::span<std::uint64_t> words_;
  std::uint64_t bit_offset_;
  std::uint64_t bit_width_;
};

class ConstLogicView {
 public:
  ConstLogicView(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words, std::uint64_t bit_offset,
      std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

 private:
  friend struct detail::PackedAccess;

  std::span<const std::uint64_t> value_words_;
  std::span<const std::uint64_t> unknown_words_;
  std::uint64_t bit_offset_;
  std::uint64_t bit_width_;
};

class LogicView {
 public:
  LogicView(
      std::span<std::uint64_t> value_words,
      std::span<std::uint64_t> unknown_words, std::uint64_t bit_offset,
      std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto AsConst() const -> ConstLogicView;

 private:
  friend struct detail::PackedAccess;

  std::span<std::uint64_t> value_words_;
  std::span<std::uint64_t> unknown_words_;
  std::uint64_t bit_offset_;
  std::uint64_t bit_width_;
};

class BitValue {
 public:
  explicit BitValue(std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto View() -> BitView;
  [[nodiscard]] auto View() const -> ConstBitView;

 private:
  PackedWords value_;
};

class LogicValue {
 public:
  explicit LogicValue(std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto View() -> LogicView;
  [[nodiscard]] auto View() const -> ConstLogicView;

 private:
  PackedWords value_;
  PackedWords unknown_;
};

}  // namespace lyra::value
