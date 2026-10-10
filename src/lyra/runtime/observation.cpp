#include "lyra/runtime/observation.hpp"

#include <cstdint>
#include <functional>
#include <memory>
#include <utility>

#include "lyra/support/event_edge.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

auto EventEdgeOf(std::int64_t edge) -> support::EventEdge {
  return static_cast<support::EventEdge>(edge);
}

auto IsEdge(
    support::EventEdge edge, value::FourStateBit before,
    value::FourStateBit now) -> bool {
  return EdgeMatches(edge, ClassifyEdge(before, now));
}

ValueWatch::ValueWatch() = default;
ValueWatch::~ValueWatch() = default;

ArmedObservation::ArmedObservation(
    std::unique_ptr<ValueWatch> watch,
    std::move_only_function<bool()> condition)
    : watch_(std::move(watch)), condition_(std::move(condition)) {
}

ArmedObservation::~ArmedObservation() = default;

Observation::Observation() = default;
Observation::Observation(const Observation&) = default;
auto Observation::operator=(const Observation&) -> Observation& = default;
Observation::Observation(Observation&&) noexcept = default;
auto Observation::operator=(Observation&&) noexcept -> Observation& = default;
Observation::~Observation() = default;

auto Observation::OnReaching() -> Observation {
  return Observation{};
}

void Observation::Arm() const {
  if (held_ != nullptr) {
    held_->Arm();
  }
}

void Observation::Disarm() const {
  if (held_ != nullptr) {
    held_->Disarm();
  }
}

auto Observation::Fires() const -> bool {
  return held_ == nullptr || held_->Fires();
}

Observation::Observation(std::shared_ptr<ArmedObservation> held)
    : held_(std::move(held)) {
}

}  // namespace lyra::runtime
