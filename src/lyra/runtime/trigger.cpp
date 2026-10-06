#include "lyra/runtime/trigger.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <span>
#include <utility>

#include "lyra/runtime/observation.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

Trigger::Trigger() = default;
Trigger::Trigger(const Trigger&) = default;
auto Trigger::operator=(const Trigger&) -> Trigger& = default;
Trigger::Trigger(Trigger&&) noexcept = default;
auto Trigger::operator=(Trigger&&) noexcept -> Trigger& = default;
Trigger::~Trigger() = default;

Trigger::Trigger(
    Observable* observable, Observation observation,
    const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width)
    : observable(observable),
      observation(std::move(observation)),
      reads{
          .lsb = static_cast<std::uint64_t>(lsb_bit_offset.ToInt64()),
          .width = static_cast<std::uint64_t>(bit_width.ToInt64())} {
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

namespace {

// The words of `plane` from `first` up to `end`, as far as the plane reaches; a
// plane a two-state value does not carry reaches none.
auto WordsOf(
    std::span<const std::uint64_t> plane, std::size_t first, std::size_t end)
    -> std::span<const std::uint64_t> {
  const std::size_t from = std::min(first, plane.size());
  return plane.subspan(from, std::min(end, plane.size()) - from);
}

}  // namespace

auto Change::Reaching(
    const value::PackedArray& storage, value::BitPositions reached) -> Change {
  Change change;
  change.reached_ = reached;
  change.first_word_ = static_cast<std::size_t>(reached.lsb / 64U);
  const std::size_t end = value::WordCountForBits(reached.lsb + reached.width);
  const auto kept = [&](std::span<const std::uint64_t> plane) {
    const std::span<const std::uint64_t> words =
        WordsOf(plane, change.first_word_, end);
    return value::PackedWordArray(words.begin(), words.end());
  };
  change.before_value_ = kept(storage.ValueWords());
  change.before_unknown_ = kept(storage.UnknownWords());
  return change;
}

void Change::SetAfter(const value::PackedArray& storage) {
  const std::size_t end =
      value::WordCountForBits(reached_.lsb + reached_.width);
  after_value_ = WordsOf(storage.ValueWords(), first_word_, end);
  after_unknown_ = WordsOf(storage.UnknownWords(), first_word_, end);
}

auto Change::BitsUnmoved(value::BitPositions at) const -> bool {
  const std::uint64_t offset = at.lsb - (std::uint64_t{first_word_} * 64U);
  return value::BitsEqual(
             {before_value_.data(), before_value_.size()}, after_value_, offset,
             at.width) &&
         value::BitsEqual(
             {before_unknown_.data(), before_unknown_.size()}, after_unknown_,
             offset, at.width);
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
