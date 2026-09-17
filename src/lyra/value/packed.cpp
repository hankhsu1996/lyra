#include "lyra/value/packed.hpp"

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/internal_error.hpp"

namespace lyra::value {

auto MaskUnusedTopBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void {
  if (bit_width == 0U) {
    throw InternalError("MaskUnusedTopBits: zero bit_width");
  }
  const std::size_t top_index = WordCountForBits(bit_width) - 1U;
  words[top_index] &= ValidBitsMask(top_index, bit_width);
}

auto SetAllValidBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void {
  for (auto& w : words) {
    w = ~std::uint64_t{0};
  }
  MaskUnusedTopBits(words, bit_width);
}

ConstBitView::ConstBitView(
    std::span<const std::uint64_t> words, std::uint64_t bit_width)
    : words_(words), bit_width_(bit_width) {
}

auto ConstBitView::ValueWords() const -> std::span<const std::uint64_t> {
  return words_;
}

auto ConstBitView::Width() const -> std::uint64_t {
  return bit_width_;
}

BitView::BitView(std::span<std::uint64_t> words, std::uint64_t bit_width)
    : words_(words), bit_width_(bit_width) {
}

auto BitView::ValueWords() const -> std::span<std::uint64_t> {
  return words_;
}

auto BitView::Width() const -> std::uint64_t {
  return bit_width_;
}

auto BitView::AsConst() const -> ConstBitView {
  return ConstBitView{
      std::span<const std::uint64_t>{words_.data(), words_.size()}, bit_width_};
}

ConstLogicView::ConstLogicView(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words, std::uint64_t bit_width)
    : value_words_(value_words),
      unknown_words_(unknown_words),
      bit_width_(bit_width) {
}

auto ConstLogicView::ValueWords() const -> std::span<const std::uint64_t> {
  return value_words_;
}

auto ConstLogicView::UnknownWords() const -> std::span<const std::uint64_t> {
  return unknown_words_;
}

auto ConstLogicView::Width() const -> std::uint64_t {
  return bit_width_;
}

LogicView::LogicView(
    std::span<std::uint64_t> value_words,
    std::span<std::uint64_t> unknown_words, std::uint64_t bit_width)
    : value_words_(value_words),
      unknown_words_(unknown_words),
      bit_width_(bit_width) {
}

auto LogicView::ValueWords() const -> std::span<std::uint64_t> {
  return value_words_;
}

auto LogicView::UnknownWords() const -> std::span<std::uint64_t> {
  return unknown_words_;
}

auto LogicView::Width() const -> std::uint64_t {
  return bit_width_;
}

auto LogicView::AsConst() const -> ConstLogicView {
  return ConstLogicView{
      std::span<const std::uint64_t>{value_words_.data(), value_words_.size()},
      std::span<const std::uint64_t>{
          unknown_words_.data(), unknown_words_.size()},
      bit_width_};
}

}  // namespace lyra::value
