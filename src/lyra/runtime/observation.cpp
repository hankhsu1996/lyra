#include "lyra/runtime/observation.hpp"

#include <memory>
#include <optional>
#include <utility>

namespace lyra::runtime {

ArmedObservation::ArmedObservation(
    std::optional<OwnedClosure<support::WatchSignature>> watch,
    std::optional<OwnedClosure<support::ConditionSignature>> condition)
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

auto Observation::OfWatch(OwnedClosure<support::WatchSignature> watch)
    -> Observation {
  return Observation{
      std::make_shared<ArmedObservation>(std::move(watch), std::nullopt)};
}

auto Observation::OfWatchQualified(
    OwnedClosure<support::WatchSignature> watch,
    OwnedClosure<support::ConditionSignature> condition) -> Observation {
  return Observation{std::make_shared<ArmedObservation>(
      std::move(watch), std::move(condition))};
}

auto Observation::Qualified(OwnedClosure<support::ConditionSignature> condition)
    -> Observation {
  return Observation{
      std::make_shared<ArmedObservation>(std::nullopt, std::move(condition))};
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
