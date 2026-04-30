#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>

#include "absl/container/inlined_vector.h"

namespace lyra::runtime {

inline constexpr std::size_t kPackedWordsInlineCapacity = 1;

using PackedWordVector =
    absl::InlinedVector<std::uint64_t, kPackedWordsInlineCapacity>;

enum class Signedness : std::uint8_t { kSigned, kUnsigned };
enum class TwoStateBit : std::uint8_t { kZero, kOne };
enum class FourStateBit : std::uint8_t {
  kZero,
  kOne,
  kHighImpedance,
  kUnknown
};

[[nodiscard]] auto WordCountForBits(std::uint64_t bit_width) -> std::size_t;

auto MaskUnusedTopBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void;

auto ValidateViewRange(
    std::size_t word_count, std::uint64_t bit_offset, std::uint64_t bit_width,
    std::string_view where) -> void;

class PackedWords {
 public:
  explicit PackedWords(std::uint64_t bit_width);

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto WordCount() const -> std::size_t;

  [[nodiscard]] auto Words() -> std::span<std::uint64_t>;
  [[nodiscard]] auto Words() const -> std::span<const std::uint64_t>;

  auto SetZero() -> void;
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
  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> TwoStateBit;
  [[nodiscard]] auto PackLowWord() const -> std::uint64_t;

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
  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> TwoStateBit;
  auto SetBit(std::uint64_t offset, TwoStateBit value) -> void;
  auto SetZero() -> void;
  auto SetOne() -> void;

  [[nodiscard]] auto AsConst() const -> ConstBitView;
  [[nodiscard]] auto PackLowWord() const -> std::uint64_t;

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
  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> FourStateBit;
  [[nodiscard]] auto PackValueLowWord() const -> std::uint64_t;
  [[nodiscard]] auto PackUnknownLowWord() const -> std::uint64_t;

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
  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> FourStateBit;
  auto SetBit(std::uint64_t offset, FourStateBit value) -> void;
  auto SetZero() -> void;
  auto SetOne() -> void;
  auto SetUnknown() -> void;
  auto SetHighImpedance() -> void;

  [[nodiscard]] auto AsConst() const -> ConstLogicView;
  [[nodiscard]] auto PackValueLowWord() const -> std::uint64_t;
  [[nodiscard]] auto PackUnknownLowWord() const -> std::uint64_t;

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
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width) -> BitView;
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width) const
      -> ConstBitView;

  auto SetZero() -> void;
  auto SetOne() -> void;

 private:
  PackedWords value_;
};

class LogicValue {
 public:
  explicit LogicValue(std::uint64_t bit_width);

  [[nodiscard]] auto Width() const -> std::uint64_t;

  [[nodiscard]] auto View() -> LogicView;
  [[nodiscard]] auto View() const -> ConstLogicView;
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width)
      -> LogicView;
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width) const
      -> ConstLogicView;

  auto SetZero() -> void;
  auto SetOne() -> void;
  auto SetUnknown() -> void;
  auto SetHighImpedance() -> void;

 private:
  PackedWords value_;
  PackedWords unknown_;
};

auto CopySameWidth(ConstBitView src, BitView dst) -> void;
auto CopySameWidth(ConstLogicView src, LogicView dst) -> void;

inline auto CopySameWidth(BitView src, BitView dst) -> void {
  CopySameWidth(src.AsConst(), dst);
}

inline auto CopySameWidth(LogicView src, LogicView dst) -> void {
  CopySameWidth(src.AsConst(), dst);
}

auto ConvertToBit(ConstBitView src, BitView dst, Signedness src_signedness)
    -> void;
auto ConvertToBit(ConstLogicView src, BitView dst, Signedness src_signedness)
    -> void;
auto ConvertToLogic(ConstBitView src, LogicView dst, Signedness src_signedness)
    -> void;
auto ConvertToLogic(
    ConstLogicView src, LogicView dst, Signedness src_signedness) -> void;

inline auto ConvertToBit(BitView src, BitView dst, Signedness src_signedness)
    -> void {
  ConvertToBit(src.AsConst(), dst, src_signedness);
}

inline auto ConvertToBit(LogicView src, BitView dst, Signedness src_signedness)
    -> void {
  ConvertToBit(src.AsConst(), dst, src_signedness);
}

inline auto ConvertToLogic(
    BitView src, LogicView dst, Signedness src_signedness) -> void {
  ConvertToLogic(src.AsConst(), dst, src_signedness);
}

inline auto ConvertToLogic(
    LogicView src, LogicView dst, Signedness src_signedness) -> void {
  ConvertToLogic(src.AsConst(), dst, src_signedness);
}

auto BitwiseNot(ConstBitView src, BitView dst) -> void;
auto BitwiseNot(ConstLogicView src, LogicView dst) -> void;

auto BitwiseAnd(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseAnd(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

auto BitwiseOr(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseOr(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

auto BitwiseXor(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseXor(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

auto BitwiseXnor(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void;
auto BitwiseXnor(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void;

inline auto BitwiseNot(BitView src, BitView dst) -> void {
  BitwiseNot(src.AsConst(), dst);
}

inline auto BitwiseNot(LogicView src, LogicView dst) -> void {
  BitwiseNot(src.AsConst(), dst);
}

inline auto BitwiseAnd(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseAnd(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseAnd(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseAnd(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseOr(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseOr(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseOr(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseOr(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXor(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseXor(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXor(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseXor(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXnor(BitView lhs, BitView rhs, BitView dst) -> void {
  BitwiseXnor(lhs.AsConst(), rhs.AsConst(), dst);
}

inline auto BitwiseXnor(LogicView lhs, LogicView rhs, LogicView dst) -> void {
  BitwiseXnor(lhs.AsConst(), rhs.AsConst(), dst);
}

}  // namespace lyra::runtime
