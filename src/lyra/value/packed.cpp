#include "lyra/value/packed.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::value {

auto MaskUnusedTopBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void {
  if (bit_width == 0U) {
    throw InternalError("MaskUnusedTopBits: zero bit_width");
  }
  const std::size_t top_index = WordCountForBits(bit_width) - 1U;
  const std::uint64_t used =
      bit_width - (static_cast<std::uint64_t>(top_index) * 64U);
  if (used == 64U) {
    return;
  }
  const std::uint64_t mask = (std::uint64_t{1} << used) - 1U;
  words[top_index] &= mask;
}

auto ValidateViewRange(
    std::size_t word_count, std::uint64_t bit_offset, std::uint64_t bit_width,
    std::string_view where) -> void {
  if (bit_width == 0U) {
    throw InternalError(std::format("{}: zero bit_width", where));
  }
  const std::uint64_t cap = static_cast<std::uint64_t>(word_count) * 64U;
  // Overflow-safe range check: never compute `bit_offset + bit_width`.
  if (bit_offset > cap || bit_width > cap - bit_offset) {
    throw InternalError(
        std::format(
            "{}: bit_offset={} bit_width={} exceeds word capacity ({} bits)",
            where, bit_offset, bit_width, cap));
  }
}

PackedWords::PackedWords(std::uint64_t bit_width)
    : bit_width_(bit_width),
      words_(WordCountForBits(bit_width), std::uint64_t{0}) {
}

auto PackedWords::BitWidth() const -> std::uint64_t {
  return bit_width_;
}

auto PackedWords::WordCount() const -> std::size_t {
  return words_.size();
}

auto PackedWords::Words() -> std::span<std::uint64_t> {
  return {words_.data(), words_.size()};
}

auto PackedWords::Words() const -> std::span<const std::uint64_t> {
  return {words_.data(), words_.size()};
}

auto PackedWords::SetOne() -> void {
  for (auto& w : words_) {
    w = ~std::uint64_t{0};
  }
  MaskUnusedTopBits(Words(), bit_width_);
}

ConstBitView::ConstBitView(
    std::span<const std::uint64_t> words, std::uint64_t bit_offset,
    std::uint64_t bit_width)
    : words_(words), bit_offset_(bit_offset), bit_width_(bit_width) {
  ValidateViewRange(words.size(), bit_offset, bit_width, "ConstBitView");
}

auto ConstBitView::Width() const -> std::uint64_t {
  return bit_width_;
}

auto ConstBitView::GetBit(std::uint64_t offset) const -> TwoStateBit {
  if (offset >= bit_width_) {
    throw InternalError("ConstBitView::GetBit: offset out of range");
  }
  const std::uint64_t abs = bit_offset_ + offset;
  const std::uint64_t word = words_[abs / 64U];
  return ((word >> (abs % 64U)) & 1U) != 0U ? TwoStateBit::kOne
                                            : TwoStateBit::kZero;
}

BitView::BitView(
    std::span<std::uint64_t> words, std::uint64_t bit_offset,
    std::uint64_t bit_width)
    : words_(words), bit_offset_(bit_offset), bit_width_(bit_width) {
  ValidateViewRange(words.size(), bit_offset, bit_width, "BitView");
}

auto BitView::Width() const -> std::uint64_t {
  return bit_width_;
}

auto BitView::GetBit(std::uint64_t offset) const -> TwoStateBit {
  return AsConst().GetBit(offset);
}

auto BitView::SetBit(std::uint64_t offset, TwoStateBit value) -> void {
  if (offset >= bit_width_) {
    throw InternalError("BitView::SetBit: offset out of range");
  }
  const std::uint64_t abs = bit_offset_ + offset;
  const std::uint64_t mask = std::uint64_t{1} << (abs % 64U);
  if (value == TwoStateBit::kOne) {
    words_[abs / 64U] |= mask;
  } else {
    words_[abs / 64U] &= ~mask;
  }
}

auto BitView::SetZero() -> void {
  for (std::uint64_t i = 0; i < bit_width_; ++i) {
    SetBit(i, TwoStateBit::kZero);
  }
}

auto BitView::AsConst() const -> ConstBitView {
  return ConstBitView{
      std::span<const std::uint64_t>{words_.data(), words_.size()}, bit_offset_,
      bit_width_};
}

ConstLogicView::ConstLogicView(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words, std::uint64_t bit_offset,
    std::uint64_t bit_width)
    : value_words_(value_words),
      unknown_words_(unknown_words),
      bit_offset_(bit_offset),
      bit_width_(bit_width) {
  if (value_words.size() != unknown_words.size()) {
    throw InternalError("ConstLogicView: plane size mismatch");
  }
  ValidateViewRange(
      value_words.size(), bit_offset, bit_width, "ConstLogicView");
}

auto ConstLogicView::Width() const -> std::uint64_t {
  return bit_width_;
}

auto ConstLogicView::GetBit(std::uint64_t offset) const -> FourStateBit {
  if (offset >= bit_width_) {
    throw InternalError("ConstLogicView::GetBit: offset out of range");
  }
  const std::uint64_t abs = bit_offset_ + offset;
  const bool vbit = ((value_words_[abs / 64U] >> (abs % 64U)) & 1U) != 0U;
  const bool ubit = ((unknown_words_[abs / 64U] >> (abs % 64U)) & 1U) != 0U;
  if (!ubit) {
    return vbit ? FourStateBit::kOne : FourStateBit::kZero;
  }
  return vbit ? FourStateBit::kUnknown : FourStateBit::kHighImpedance;
}

LogicView::LogicView(
    std::span<std::uint64_t> value_words,
    std::span<std::uint64_t> unknown_words, std::uint64_t bit_offset,
    std::uint64_t bit_width)
    : value_words_(value_words),
      unknown_words_(unknown_words),
      bit_offset_(bit_offset),
      bit_width_(bit_width) {
  if (value_words.size() != unknown_words.size()) {
    throw InternalError("LogicView: plane size mismatch");
  }
  ValidateViewRange(value_words.size(), bit_offset, bit_width, "LogicView");
}

auto LogicView::Width() const -> std::uint64_t {
  return bit_width_;
}

auto LogicView::GetBit(std::uint64_t offset) const -> FourStateBit {
  return AsConst().GetBit(offset);
}

auto LogicView::SetBit(std::uint64_t offset, FourStateBit value) -> void {
  if (offset >= bit_width_) {
    throw InternalError("LogicView::SetBit: offset out of range");
  }
  const std::uint64_t abs = bit_offset_ + offset;
  const std::uint64_t mask = std::uint64_t{1} << (abs % 64U);
  const bool vbit =
      (value == FourStateBit::kOne) || (value == FourStateBit::kUnknown);
  const bool ubit = (value == FourStateBit::kHighImpedance) ||
                    (value == FourStateBit::kUnknown);
  if (vbit) {
    value_words_[abs / 64U] |= mask;
  } else {
    value_words_[abs / 64U] &= ~mask;
  }
  if (ubit) {
    unknown_words_[abs / 64U] |= mask;
  } else {
    unknown_words_[abs / 64U] &= ~mask;
  }
}

auto LogicView::SetZero() -> void {
  for (std::uint64_t i = 0; i < bit_width_; ++i) {
    SetBit(i, FourStateBit::kZero);
  }
}

auto LogicView::AsConst() const -> ConstLogicView {
  return ConstLogicView{
      std::span<const std::uint64_t>{value_words_.data(), value_words_.size()},
      std::span<const std::uint64_t>{
          unknown_words_.data(), unknown_words_.size()},
      bit_offset_, bit_width_};
}

BitValue::BitValue(std::uint64_t bit_width) : value_(bit_width) {
}

auto BitValue::Width() const -> std::uint64_t {
  return value_.BitWidth();
}

auto BitValue::View() -> BitView {
  return BitView{value_.Words(), 0U, value_.BitWidth()};
}

auto BitValue::View() const -> ConstBitView {
  return ConstBitView{value_.Words(), 0U, value_.BitWidth()};
}

auto BitValue::View(std::uint64_t offset, std::uint64_t width) -> BitView {
  ValidateViewRange(value_.WordCount(), offset, width, "BitValue::View");
  return BitView{value_.Words(), offset, width};
}

auto BitValue::View(std::uint64_t offset, std::uint64_t width) const
    -> ConstBitView {
  ValidateViewRange(value_.WordCount(), offset, width, "BitValue::View");
  return ConstBitView{value_.Words(), offset, width};
}

LogicValue::LogicValue(std::uint64_t bit_width)
    : value_(bit_width), unknown_(bit_width) {
  value_.SetOne();
  unknown_.SetOne();
}

auto LogicValue::Width() const -> std::uint64_t {
  return value_.BitWidth();
}

auto LogicValue::View() -> LogicView {
  return LogicView{value_.Words(), unknown_.Words(), 0U, value_.BitWidth()};
}

auto LogicValue::View() const -> ConstLogicView {
  return ConstLogicView{
      value_.Words(), unknown_.Words(), 0U, value_.BitWidth()};
}

auto LogicValue::View(std::uint64_t offset, std::uint64_t width) -> LogicView {
  ValidateViewRange(value_.WordCount(), offset, width, "LogicValue::View");
  return LogicView{value_.Words(), unknown_.Words(), offset, width};
}

auto LogicValue::View(std::uint64_t offset, std::uint64_t width) const
    -> ConstLogicView {
  ValidateViewRange(value_.WordCount(), offset, width, "LogicValue::View");
  return ConstLogicView{value_.Words(), unknown_.Words(), offset, width};
}

}  // namespace lyra::value
