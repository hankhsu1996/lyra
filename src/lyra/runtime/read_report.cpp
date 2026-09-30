#include "lyra/runtime/read_report.hpp"

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

namespace {

// How deep reports may nest before a report stops following calls. It bounds
// only a cycle of calls: no program without one nests reports past the depth
// its own calls nest, which is far below this.
constexpr std::int64_t kReportDepthBound = 64;

}  // namespace

ReadReport::ReadReport(Observation observation)
    : observation_(std::move(observation)) {
}

auto ReadReport::For(Observation observation) -> ReadReport {
  return ReadReport{std::move(observation)};
}

ReadReport::ReadReport(ReadReport&&) noexcept = default;
auto ReadReport::operator=(ReadReport&&) noexcept -> ReadReport& = default;
ReadReport::~ReadReport() = default;

void ReadReport::Add(
    Observable* place, const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width) {
  triggers_.emplace_back(place, observation_, lsb_bit_offset, bit_width);
}

void ReadReport::AddEveryObject() {
  Trigger trigger;
  trigger.observable = &current_runtime().EveryObject();
  trigger.observation = observation_;
  triggers_.push_back(std::move(trigger));
}

auto ReadReport::Enter() -> std::int64_t {
  if (depth_ == kReportDepthBound) {
    AddEveryObject();
    return 0;
  }
  ++depth_;
  return 1;
}

void ReadReport::Leave() {
  --depth_;
}

void RefuseReport(std::string_view why) {
  throw SimulationError(
      "waiting on an expression that calls this function is not yet "
      "supported: " +
      std::string{why});
}

}  // namespace lyra::runtime
