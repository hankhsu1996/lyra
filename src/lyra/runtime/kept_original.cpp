#include "lyra/runtime/kept_original.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <optional>
#include <span>

#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

KeptOriginal::KeptOriginal() = default;

KeptOriginal::~KeptOriginal() = default;

KeptBits::KeptBits(value::IntegralView live) : live_(live) {
}

void KeptBits::Keep(value::BitPositions bits) {
  const auto first = static_cast<std::size_t>(bits.lsb / 64U);
  const auto last = static_cast<std::size_t>((bits.lsb + bits.width - 1) / 64U);
  if (value_.empty()) {
    first_ = first;
    Take(first, last + 1, false);
    return;
  }
  if (first < first_) {
    Take(first, first_, true);
    first_ = first;
  }
  const std::size_t end = first_ + value_.size();
  if (last >= end) {
    Take(end, last + 1, false);
  }
}

// Takes the words from `from` up to `to` of each plane as they stand, before
// the words already kept or after them.
void KeptBits::Take(std::size_t from, std::size_t to, bool before) {
  const auto taken = [&](std::deque<std::uint64_t>& kept,
                         std::span<std::uint64_t> plane) {
    kept.insert(
        before ? kept.begin() : kept.end(),
        plane.begin() + static_cast<std::ptrdiff_t>(from),
        plane.begin() + static_cast<std::ptrdiff_t>(to));
  };
  taken(value_, live_.value);
  if (live_.IsFourState()) {
    taken(unknown_, live_.unknown);
  }
}

// The kept words are put back while the change is measured, which reads the
// bits before from where they lie, and taken out again after.
auto KeptBits::Changed() -> std::optional<Change> {
  if (value_.empty()) {
    return std::nullopt;
  }
  const std::uint64_t lsb = std::uint64_t{first_} * 64U;
  const value::BitPositions kept{
      .lsb = lsb,
      .width = std::min<std::uint64_t>(value_.size() * 64U, live_.width - lsb)};
  Exchange();
  Change change = Change::Reaching(live_.AsConst(), kept);
  Exchange();
  change.SetAfter(live_.AsConst());
  if (change.Unmoved()) {
    return std::nullopt;
  }
  return change;
}

void KeptBits::Exchange() {
  const auto offset = static_cast<std::ptrdiff_t>(first_);
  std::swap_ranges(value_.begin(), value_.end(), live_.value.begin() + offset);
  if (live_.IsFourState()) {
    std::swap_ranges(
        unknown_.begin(), unknown_.end(), live_.unknown.begin() + offset);
  }
}

auto ByStep::operator()(const PartPath& a, const PartPath& b) const -> bool {
  return std::ranges::lexicographical_compare(a.Steps(), b.Steps());
}

}  // namespace lyra::runtime
