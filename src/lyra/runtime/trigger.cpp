#include "lyra/runtime/trigger.hpp"

#include <cstdint>
#include <utility>

#include "lyra/runtime/observation.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

Trigger::Trigger(
    Observable* observable, Observation observation,
    const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width)
    : observable(observable),
      observation(std::move(observation)),
      lsb_bit_offset(static_cast<std::uint64_t>(lsb_bit_offset.ToInt64())),
      bit_width(static_cast<std::uint64_t>(bit_width.ToInt64())) {
}

}  // namespace lyra::runtime
