#include "lyra/runtime/trigger.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <span>
#include <utility>

#include "lyra/base/fixed_array.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

Trigger::Trigger() = default;
Trigger::Trigger(const Trigger&) = default;
auto Trigger::operator=(const Trigger&) -> Trigger& = default;
Trigger::Trigger(Trigger&&) noexcept = default;
auto Trigger::operator=(Trigger&&) noexcept -> Trigger& = default;
Trigger::~Trigger() = default;

Trigger::Trigger(
    Observable* observable, Observation observation,
    std::int64_t lsb_bit_offset, std::int64_t bit_width)
    : observable(observable),
      observation(std::move(observation)),
      reads{
          .lsb = static_cast<std::uint64_t>(lsb_bit_offset),
          .width = static_cast<std::uint64_t>(bit_width)} {
}

Change::Change() = default;
Change::Change(const Change&) = default;
Change::Change(Change&&) noexcept = default;
auto Change::operator=(const Change&) -> Change& = default;
auto Change::operator=(Change&&) noexcept -> Change& = default;
Change::~Change() = default;

auto Change::Whole() -> Change {
  return Change{};
}

auto Change::WordsOf(std::size_t run) -> std::span<std::uint64_t> {
  return std::span<std::uint64_t>(words_).subspan(run * count_, count_);
}

auto Change::WordsOf(std::size_t run) const -> std::span<const std::uint64_t> {
  return std::span<const std::uint64_t>(words_).subspan(run * count_, count_);
}

namespace {

// The words of `plane` from `first` on copied into `kept`, as far as the plane
// reaches; a plane a two-state value does not carry reaches none.
void KeepWords(
    std::span<const std::uint64_t> plane, std::size_t first,
    std::span<std::uint64_t> kept) {
  const std::size_t from = std::min(first, plane.size());
  const std::span<const std::uint64_t> words =
      plane.subspan(from, std::min(kept.size(), plane.size() - from));
  std::ranges::copy(words, kept.begin());
}

}  // namespace

auto Change::Reaching(value::ConstPlanes storage, value::BitPositions reached)
    -> Change {
  Change change;
  change.reached_ = reached;
  change.first_word_ = static_cast<std::size_t>(reached.lsb / 64U);
  change.count_ =
      value::WordCountForBits(reached.lsb + reached.width) - change.first_word_;
  change.words_ = base::FixedArray<std::uint64_t, 4>(4 * change.count_);
  KeepWords(storage.value, change.first_word_, change.WordsOf(kBeforeValue));
  KeepWords(
      storage.unknown, change.first_word_, change.WordsOf(kBeforeUnknown));
  return change;
}

void Change::SetAfter(value::ConstPlanes storage) {
  KeepWords(storage.value, first_word_, WordsOf(kAfterValue));
  KeepWords(storage.unknown, first_word_, WordsOf(kAfterUnknown));
}

auto Change::ReachingInWord(
    std::uint64_t value, std::uint64_t unknown, value::BitPositions reached)
    -> Change {
  Change change;
  change.reached_ = reached;
  change.count_ = 1;
  change.words_ = base::FixedArray<std::uint64_t, 4>(4);
  change.words_[kBeforeValue] = value;
  change.words_[kBeforeUnknown] = unknown;
  return change;
}

void Change::SetAfterInWord(std::uint64_t value, std::uint64_t unknown) {
  words_[kAfterValue] = value;
  words_[kAfterUnknown] = unknown;
}

auto Change::BitsUnmoved(value::BitPositions at) const -> bool {
  const std::span<const std::uint64_t> before_value = WordsOf(kBeforeValue);
  const std::span<const std::uint64_t> before_unknown = WordsOf(kBeforeUnknown);
  const std::span<const std::uint64_t> after_value = WordsOf(kAfterValue);
  const std::span<const std::uint64_t> after_unknown = WordsOf(kAfterUnknown);
  const std::uint64_t from = at.lsb - (std::uint64_t{first_word_} * 64U);
  const std::uint64_t to = from + at.width;
  for (std::uint64_t position = from; position < to;) {
    const auto word = static_cast<std::size_t>(position / 64U);
    const std::uint64_t offset = position % 64U;
    const std::uint64_t count =
        std::min<std::uint64_t>(64U - offset, to - position);
    const std::uint64_t moved = (before_value[word] ^ after_value[word]) |
                                (before_unknown[word] ^ after_unknown[word]);
    const std::uint64_t compared = value::LowBits(count) << offset;
    if ((moved & compared) != 0U) {
      return false;
    }
    position += count;
  }
  return true;
}

auto Change::Unmoved() const -> bool {
  return reached_.width != 0 && BitsUnmoved(reached_);
}

auto Change::KnownUnchanged(value::BitPositions reads) const -> bool {
  if (reached_.width == 0) {
    return false;
  }
  const std::uint64_t reached_end = reached_.lsb + reached_.width;
  const std::uint64_t read_end = reads.width == 0
                                     ? std::numeric_limits<std::uint64_t>::max()
                                     : reads.lsb + reads.width;
  const std::uint64_t from = std::max(reads.lsb, reached_.lsb);
  const std::uint64_t to = std::min(read_end, reached_end);
  if (from >= to) {
    return true;
  }
  if (from == reached_.lsb && to == reached_end) {
    return false;
  }
  return BitsUnmoved({.lsb = from, .width = to - from});
}

}  // namespace lyra::runtime
