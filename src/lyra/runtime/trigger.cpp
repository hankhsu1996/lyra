#include "lyra/runtime/trigger.hpp"

#include <cstdint>
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
      lsb_bit_offset(static_cast<std::uint64_t>(lsb_bit_offset.ToInt64())),
      bit_width(static_cast<std::uint64_t>(bit_width.ToInt64())) {
}

Change::Change() = default;

auto Change::Whole() -> Change {
  return Change{};
}

auto Change::Between(
    const value::PackedArray& before, const value::PackedArray& after)
    -> Change {
  Change change;
  change.before_value_ = before.ValueWords();
  change.before_unknown_ = before.UnknownWords();
  change.after_value_ = after.ValueWords();
  change.after_unknown_ = after.UnknownWords();
  change.bit_width_ = after.BitWidth();
  return change;
}

auto Change::LeftAlone(
    std::uint64_t lsb_bit_offset, std::uint64_t bit_width) const -> bool {
  const bool reads_every_bit =
      bit_width == 0 || (lsb_bit_offset == 0 && bit_width >= bit_width_);
  if (bit_width_ == 0 || reads_every_bit) {
    return false;
  }
  return value::BitRunsEqual(
             before_value_, after_value_, lsb_bit_offset, bit_width) &&
         value::BitRunsEqual(
             before_unknown_, after_unknown_, lsb_bit_offset, bit_width);
}

}  // namespace lyra::runtime
