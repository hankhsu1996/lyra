#include "lyra/runtime/trigger.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <optional>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/fixed_array.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

Trigger::Trigger() = default;
Trigger::Trigger(const Trigger&) = default;
auto Trigger::operator=(const Trigger&) -> Trigger& = default;
Trigger::Trigger(Trigger&&) noexcept = default;
auto Trigger::operator=(Trigger&&) noexcept -> Trigger& = default;
Trigger::~Trigger() = default;

auto PositionsInPart(std::int64_t first, std::int64_t count)
    -> value::BitPositions {
  if (first < 0 && first + count > 0) {
    return {.lsb = 0, .width = static_cast<std::uint64_t>(first + count)};
  }
  return {
      .lsb = static_cast<std::uint64_t>(first),
      .width = static_cast<std::uint64_t>(count)};
}

Trigger::Trigger(
    WatchedPlace place, Observation observation,
    std::span<const std::int64_t> steps, std::int64_t lsb_bit_offset,
    std::int64_t bit_width)
    : observable(place.ReportedTo()),
      through(place.Member()),
      part(place.PathOf(steps)),
      observation(std::move(observation)) {
  // A path that stops short of the part watches all of what it reaches, since
  // the positions given are the part's and not that.
  if (!part.IsInexact()) {
    reads = PositionsInPart(lsb_bit_offset, bit_width);
  }
}

Change::Change(Shape shape) : shape_(std::move(shape)) {
}
Change::Change(const Change&) = default;
Change::Change(Change&&) noexcept = default;
auto Change::operator=(const Change&) -> Change& = default;
auto Change::operator=(Change&&) noexcept -> Change& = default;
Change::~Change() = default;

auto Change::Whole() -> Change {
  return Change(Unknown{});
}

auto Change::Replacing(
    const value::ValueType& type, const void* before, const void* after)
    -> Change {
  return Change(ReplacedValue{.type = &type, .before = before, .after = after});
}

auto Change::Elements(value::BitPositions reached) -> Change {
  return Change(ElementRun{.reached = reached});
}

auto Change::BitWords::WordsOf(std::size_t run) -> std::span<std::uint64_t> {
  return std::span<std::uint64_t>(words).subspan(run * count, count);
}

auto Change::BitWords::WordsOf(std::size_t run) const
    -> std::span<const std::uint64_t> {
  return std::span<const std::uint64_t>(words).subspan(run * count, count);
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

auto Change::Reaching(
    const value::ConstIntegralView& storage, value::BitPositions reached)
    -> Change {
  const auto first_word = static_cast<std::size_t>(reached.lsb / 64U);
  const std::size_t count =
      value::WordCountForBits(reached.lsb + reached.width) - first_word;
  BitWords bits{
      .reached = reached,
      .first_word = first_word,
      .count = count,
      .words = base::FixedArray<std::uint64_t, 4>(4 * count)};
  KeepWords(
      storage.value, bits.first_word, bits.WordsOf(BitWords::kBeforeValue));
  KeepWords(
      storage.unknown, bits.first_word, bits.WordsOf(BitWords::kBeforeUnknown));
  return Change(std::move(bits));
}

void Change::SetAfter(const value::ConstIntegralView& storage) {
  auto& bits = std::get<BitWords>(shape_);
  KeepWords(
      storage.value, bits.first_word, bits.WordsOf(BitWords::kAfterValue));
  KeepWords(
      storage.unknown, bits.first_word, bits.WordsOf(BitWords::kAfterUnknown));
}

auto Change::ReachingInWord(
    std::uint64_t value, std::uint64_t unknown, value::BitPositions reached)
    -> Change {
  BitWords bits{
      .reached = reached,
      .first_word = 0,
      .count = 1,
      .words = base::FixedArray<std::uint64_t, 4>(4)};
  bits.words[BitWords::kBeforeValue] = value;
  bits.words[BitWords::kBeforeUnknown] = unknown;
  return Change(std::move(bits));
}

void Change::SetAfterInWord(std::uint64_t value, std::uint64_t unknown) {
  auto& bits = std::get<BitWords>(shape_);
  bits.words[BitWords::kAfterValue] = value;
  bits.words[BitWords::kAfterUnknown] = unknown;
}

auto Change::BitWords::Unmoved(value::BitPositions at) const -> bool {
  const std::span<const std::uint64_t> before_value = WordsOf(kBeforeValue);
  const std::span<const std::uint64_t> before_unknown = WordsOf(kBeforeUnknown);
  const std::span<const std::uint64_t> after_value = WordsOf(kAfterValue);
  const std::span<const std::uint64_t> after_unknown = WordsOf(kAfterUnknown);
  const std::uint64_t from = at.lsb - (std::uint64_t{first_word} * 64U);
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
  return std::visit(
      Overloaded{
          [](const BitWords& bits) { return bits.Unmoved(bits.reached); },
          [](const Unknown&) { return false; },
          [](const ElementRun&) { return false; },
          [](const ReplacedValue&) { return false; },
      },
      shape_);
}

namespace {

// The positions `reads` shares with `reached`, a width of zero reading every
// position, and none where they share none.
auto Shared(value::BitPositions reached, value::BitPositions reads)
    -> std::optional<value::BitPositions> {
  const std::uint64_t reached_end = reached.lsb + reached.width;
  const std::uint64_t read_end = reads.width == 0
                                     ? std::numeric_limits<std::uint64_t>::max()
                                     : reads.lsb + reads.width;
  const std::uint64_t from = std::max(reads.lsb, reached.lsb);
  const std::uint64_t to = std::min(read_end, reached_end);
  if (from >= to) {
    return std::nullopt;
  }
  return value::BitPositions{.lsb = from, .width = to - from};
}

// What became of a part between `was` and `now`, the part one step names
// before and after: none where neither holds one, every wait on it asked where
// one of them does not, and otherwise what the two values say.
auto ChangeOfPart(value::ValuePart was, value::ValuePart now)
    -> std::optional<Change> {
  if (was.at == nullptr || now.at == nullptr) {
    if (was.at == now.at) {
      return std::nullopt;
    }
    return Change::Whole();
  }
  return Change::Between(*now.type, was.at, now.at);
}

}  // namespace

auto Change::KnownUnchanged(value::BitPositions reads) const -> bool {
  // A change of a value or of a run of elements reads the parts it is asked
  // of; one read whole is moved, since it is told only where it moved.
  const auto every_part_unmoved = [&] {
    if (reads.width == 0) {
      return false;
    }
    for (std::uint64_t step = reads.lsb; step < reads.lsb + reads.width;
         ++step) {
      if (Inside(static_cast<std::uint32_t>(step)).has_value()) {
        return false;
      }
    }
    return true;
  };
  return std::visit(
      Overloaded{
          [](const Unknown&) { return false; },
          [&](const ElementRun& run) {
            return run.reached.width != 0 &&
                   !Shared(run.reached, reads).has_value();
          },
          [&](const BitWords& bits) {
            const std::optional<value::BitPositions> shared =
                Shared(bits.reached, reads);
            if (!shared.has_value()) {
              return true;
            }
            // A change is told only where what it reached moved, so a read of
            // all of it is moved.
            const bool reads_all = shared->lsb == bits.reached.lsb &&
                                   shared->width == bits.reached.width;
            return !reads_all && bits.Unmoved(*shared);
          },
          [&](const ReplacedValue&) { return every_part_unmoved(); },
      },
      shape_);
}

auto Change::Inside(std::uint32_t step) const -> std::optional<Change> {
  const value::BitPositions only{.lsb = step, .width = 1};
  const auto by_reach = [&]() -> std::optional<Change> {
    if (KnownUnchanged(only)) {
      return std::nullopt;
    }
    return Whole();
  };
  return std::visit(
      Overloaded{
          [](const Unknown&) -> std::optional<Change> { return Whole(); },
          [&](const ElementRun&) { return by_reach(); },
          [&](const BitWords&) { return by_reach(); },
          [&](const ReplacedValue& value) -> std::optional<Change> {
            if (!value.type->HasSteps()) {
              return Whole();
            }
            return ChangeOfPart(
                value.type->PartAtStep(value.before, step),
                value.type->PartAtStep(value.after, step));
          }},
      shape_);
}

auto Change::Between(
    const value::ValueType& type, const void* before, const void* after)
    -> std::optional<Change> {
  if (const value::IntegralValueType* integral = type.AsIntegral()) {
    const value::LoadedWords kept = integral->Load(before);
    Change change = Reaching(
        kept.View(),
        value::BitPositions{.lsb = 0, .width = integral->Shape().width});
    change.SetAfter(integral->Load(after).View());
    if (change.Unmoved()) {
      return std::nullopt;
    }
    return change;
  }
  if (type.BitIdentical(before, after)) {
    return std::nullopt;
  }
  return Replacing(type, before, after);
}

}  // namespace lyra::runtime
