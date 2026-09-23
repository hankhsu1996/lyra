#include "lyra/runtime/observation.hpp"

#include <memory>
#include <utility>

#include "lyra/support/event_edge.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::runtime {

auto Settled(const value::ObjectRef& reference) -> value::RuntimeValue {
  return value::RuntimeValue{reference.Handle()};
}

template auto Settled<value::PackedArray>(value::PackedArray)
    -> value::RuntimeValue;
template auto Settled<value::String>(value::String) -> value::RuntimeValue;
template auto Settled<value::Real>(value::Real) -> value::RuntimeValue;
template auto Settled<value::ShortReal>(value::ShortReal)
    -> value::RuntimeValue;
template auto Settled<value::Chandle>(value::Chandle) -> value::RuntimeValue;

ValueWatch::~ValueWatch() = default;

ArmedObservation::~ArmedObservation() = default;

auto ArmedObservation::EdgeOf(const value::PackedArray& edge)
    -> support::EventEdge {
  return static_cast<support::EventEdge>(edge.ToInt64());
}

Observation::Observation() = default;
Observation::Observation(const Observation&) = default;
auto Observation::operator=(const Observation&) -> Observation& = default;
Observation::Observation(Observation&&) noexcept = default;
auto Observation::operator=(Observation&&) noexcept -> Observation& = default;
Observation::~Observation() = default;

auto Observation::OnReaching() -> Observation {
  return Observation{};
}

Observation::Observation(std::shared_ptr<ArmedObservation> held)
    : held_(std::move(held)) {
}

}  // namespace lyra::runtime
