#include "lyra/runtime/read_report.hpp"

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

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

ReadReport::ReadReport() = default;

auto ReadReport::Empty() -> ReadReport {
  return ReadReport{};
}

ReadReport::ReadReport(ReadReport&&) noexcept = default;
auto ReadReport::operator=(ReadReport&&) noexcept -> ReadReport& = default;
ReadReport::~ReadReport() = default;

// A place is watched only for being reached: the process decides by its own
// evaluation, so nothing is asked where the change happens.
void ReadReport::Add(
    Observable* place, const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width) {
  triggers_.emplace_back(
      place, Observation::OnReaching(), lsb_bit_offset, bit_width);
}

void ReadReport::AddEveryObject() {
  Trigger trigger;
  trigger.observable = &current_runtime().EveryObject();
  triggers_.push_back(std::move(trigger));
}

auto ReadReport::TakeTriggers() -> std::vector<Trigger> {
  return std::exchange(triggers_, {});
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

// A report a function makes nests in the report of whatever called it, so the
// call the evaluation made is the one that leaves nothing open.
auto ReadReport::RunsTheBody() const -> std::int64_t {
  return depth_ == 0 ? 1 : 0;
}

void RefuseReport(std::string_view why) {
  throw SimulationError(
      "waiting on an expression that calls this function is not yet "
      "supported: " +
      std::string{why});
}

}  // namespace lyra::runtime
