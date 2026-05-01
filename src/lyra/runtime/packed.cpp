#include "lyra/runtime/packed.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

auto WordCountForBits(std::uint64_t bit_width) -> std::size_t {
  if (bit_width == 0U) {
    throw InternalError("WordCountForBits: zero bit_width");
  }
  // Overflow-safe form: never compute `bit_width + 63`.
  const std::uint64_t whole = bit_width / 64U;
  const std::uint64_t remainder = bit_width % 64U;
  return static_cast<std::size_t>(whole + (remainder == 0U ? 0U : 1U));
}

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
  if (bit_width == 0U) {
    throw InternalError("PackedWords: zero bit_width");
  }
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

auto PackedWords::SetZero() -> void {
  for (auto& w : words_) {
    w = 0U;
  }
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

auto BitView::SetOne() -> void {
  for (std::uint64_t i = 0; i < bit_width_; ++i) {
    SetBit(i, TwoStateBit::kOne);
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

auto LogicView::SetOne() -> void {
  for (std::uint64_t i = 0; i < bit_width_; ++i) {
    SetBit(i, FourStateBit::kOne);
  }
}

auto LogicView::SetUnknown() -> void {
  for (std::uint64_t i = 0; i < bit_width_; ++i) {
    SetBit(i, FourStateBit::kUnknown);
  }
}

auto LogicView::SetHighImpedance() -> void {
  for (std::uint64_t i = 0; i < bit_width_; ++i) {
    SetBit(i, FourStateBit::kHighImpedance);
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

auto BitValue::SetZero() -> void {
  value_.SetZero();
}

auto BitValue::SetOne() -> void {
  value_.SetOne();
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

auto LogicValue::SetZero() -> void {
  View().SetZero();
}

auto LogicValue::SetOne() -> void {
  View().SetOne();
}

auto LogicValue::SetUnknown() -> void {
  View().SetUnknown();
}

auto LogicValue::SetHighImpedance() -> void {
  View().SetHighImpedance();
}

auto CopySameWidth(ConstBitView src, BitView dst) -> void {
  if (src.Width() != dst.Width()) {
    throw InternalError("CopySameWidth(Bit): width mismatch");
  }
  PackedWordVector tmp(WordCountForBits(src.Width()), std::uint64_t{0});
  BitView tmp_view{
      std::span<std::uint64_t>{tmp.data(), tmp.size()}, 0U, src.Width()};
  for (std::uint64_t i = 0; i < src.Width(); ++i) {
    tmp_view.SetBit(i, src.GetBit(i));
  }
  const ConstBitView ctv = tmp_view.AsConst();
  for (std::uint64_t i = 0; i < src.Width(); ++i) {
    dst.SetBit(i, ctv.GetBit(i));
  }
}

auto CopySameWidth(ConstLogicView src, LogicView dst) -> void {
  if (src.Width() != dst.Width()) {
    throw InternalError("CopySameWidth(Logic): width mismatch");
  }
  PackedWordVector vt(WordCountForBits(src.Width()), std::uint64_t{0});
  PackedWordVector ut(WordCountForBits(src.Width()), std::uint64_t{0});
  LogicView tmp_view{
      std::span<std::uint64_t>{vt.data(), vt.size()},
      std::span<std::uint64_t>{ut.data(), ut.size()}, 0U, src.Width()};
  for (std::uint64_t i = 0; i < src.Width(); ++i) {
    tmp_view.SetBit(i, src.GetBit(i));
  }
  const ConstLogicView ctv = tmp_view.AsConst();
  for (std::uint64_t i = 0; i < src.Width(); ++i) {
    dst.SetBit(i, ctv.GetBit(i));
  }
}

}  // namespace lyra::runtime
