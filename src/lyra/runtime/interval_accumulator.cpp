#include "lyra/runtime/interval_accumulator.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

IntervalOriginal::IntervalOriginal() = default;

IntervalOriginal::~IntervalOriginal() = default;

void BitsOriginal::Words::Insert(
    std::span<const std::uint64_t> words, bool before) {
  const std::size_t grown = size_ + words.size();
  const std::span<const std::uint64_t> kept = Span();
  const auto [front, back] =
      before ? std::pair{words, kept} : std::pair{kept, words};
  if (grown <= kRoom) {
    std::array<std::uint64_t, kRoom> merged{};
    const std::span<std::uint64_t> into(merged);
    std::ranges::copy(front, into.begin());
    std::ranges::copy(back, into.subspan(front.size()).begin());
    inline_ = merged;
  } else {
    std::vector<std::uint64_t> merged;
    merged.reserve(grown);
    merged.insert(merged.end(), front.begin(), front.end());
    merged.insert(merged.end(), back.begin(), back.end());
    spilled_ = std::move(merged);
  }
  size_ = grown;
}

BitsOriginal::BitsOriginal(value::IntegralView live) : live_(live) {
}

void BitsOriginal::Keep(value::BitPositions bits) {
  const auto first = static_cast<std::size_t>(bits.lsb / 64U);
  const auto last = static_cast<std::size_t>((bits.lsb + bits.width - 1) / 64U);
  if (value_.Size() == 0) {
    first_ = first;
    Take(first, last + 1, false);
    return;
  }
  if (first < first_) {
    Take(first, first_, true);
    first_ = first;
  }
  const std::size_t end = first_ + value_.Size();
  if (last >= end) {
    Take(end, last + 1, false);
  }
}

// Takes the words from `from` up to `to` of each plane as they stand, before
// the words already kept or after them.
void BitsOriginal::Take(std::size_t from, std::size_t to, bool before) {
  const auto taken = [&](Words& kept, std::span<std::uint64_t> plane) {
    kept.Insert(plane.subspan(from, to - from), before);
  };
  taken(value_, live_.value);
  if (live_.IsFourState()) {
    taken(unknown_, live_.unknown);
  }
}

// The kept words are put back while the change is measured, which reads the
// bits before from where they lie, and taken out again after.
auto BitsOriginal::Changed() -> std::optional<Change> {
  if (value_.Size() == 0) {
    return std::nullopt;
  }
  const std::uint64_t lsb = std::uint64_t{first_} * 64U;
  const value::BitPositions kept{
      .lsb = lsb,
      .width = std::min<std::uint64_t>(value_.Size() * 64U, live_.width - lsb)};
  Exchange();
  Change change = Change::Reaching(live_.AsConst(), kept);
  Exchange();
  change.SetAfter(live_.AsConst());
  if (change.Unmoved()) {
    return std::nullopt;
  }
  return change;
}

void BitsOriginal::Exchange() {
  const auto exchanged = [&](Words& kept, std::span<std::uint64_t> plane) {
    std::ranges::swap_ranges(kept.Span(), plane.subspan(first_, kept.Size()));
  };
  exchanged(value_, live_.value);
  if (live_.IsFourState()) {
    exchanged(unknown_, live_.unknown);
  }
}

auto ByStep::operator()(const PartPath& a, const PartPath& b) const -> bool {
  return std::ranges::lexicographical_compare(a.Steps(), b.Steps());
}

}  // namespace lyra::runtime
