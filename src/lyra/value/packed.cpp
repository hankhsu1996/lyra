#include "lyra/value/packed.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>

namespace lyra::value {

auto SetAllValidBits(std::span<std::uint64_t> words, std::uint64_t bit_width)
    -> void {
  for (auto& w : words) {
    w = ~std::uint64_t{0};
  }
  MaskUnusedTopBits(words, bit_width);
}

namespace {

constexpr std::uint64_t kWordBits = 64U;

// `count` positions, right-aligned. A step of a run covers at least one
// position and at most a whole word, and a whole word is the case a shift
// cannot express.
auto LowBits(std::uint64_t count) -> std::uint64_t {
  return count == kWordBits ? ~std::uint64_t{0}
                            : (std::uint64_t{1} << count) - 1U;
}

// One word's worth of `words` beginning at `offset`, right-aligned, keeping
// `count` positions. A position the span does not reach reads as clear, which
// is what an absent plane answers for every one of its bits.
auto BitsAt(
    std::span<const std::uint64_t> words, std::uint64_t offset,
    std::uint64_t count) -> std::uint64_t {
  const auto at = [words](std::uint64_t index) -> std::uint64_t {
    return index < words.size() ? words[static_cast<std::size_t>(index)]
                                : std::uint64_t{0};
  };
  const std::uint64_t word = offset / kWordBits;
  const std::uint64_t shift = offset % kWordBits;
  std::uint64_t bits = at(word) >> shift;
  if (shift != 0U) {
    bits |= at(word + 1U) << (kWordBits - shift);
  }
  return bits & LowBits(count);
}

// Where a run's next step lands: which destination word, how far into it, and
// how many positions of that word the step covers.
struct RunStep {
  std::size_t word;
  std::uint64_t offset;
  std::uint64_t count;
};

auto StepAt(std::uint64_t position, std::uint64_t remaining) -> RunStep {
  const std::uint64_t offset = position % kWordBits;
  return RunStep{
      .word = static_cast<std::size_t>(position / kWordBits),
      .offset = offset,
      .count = std::min(kWordBits - offset, remaining)};
}

}  // namespace

auto MoveBitRun(
    std::span<const std::uint64_t> src, std::uint64_t src_offset,
    std::span<std::uint64_t> dst, std::uint64_t dst_offset, std::uint64_t count)
    -> void {
  for (std::uint64_t moved = 0U; moved < count;) {
    const RunStep step = StepAt(dst_offset + moved, count - moved);
    const std::uint64_t mask = LowBits(step.count) << step.offset;
    const std::uint64_t bits = BitsAt(src, src_offset + moved, step.count);
    dst[step.word] = (dst[step.word] & ~mask) | (bits << step.offset);
    moved += step.count;
  }
}

auto SetBitRun(
    std::span<std::uint64_t> dst, std::uint64_t offset, std::uint64_t count)
    -> void {
  for (std::uint64_t set = 0U; set < count;) {
    const RunStep step = StepAt(offset + set, count - set);
    dst[step.word] |= LowBits(step.count) << step.offset;
    set += step.count;
  }
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
