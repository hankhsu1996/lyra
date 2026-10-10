#include "lyra/runtime/named_event.hpp"

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::runtime {

NamedEvent::NamedEvent() = default;
NamedEvent::~NamedEvent() = default;

void NamedEvent::Trigger(RuntimeEffects& runtime) {
  last_triggered_at_ = runtime.Now();
  runtime.WakeParkedOn(Members(), Change::Whole());
}

auto NamedEvent::Triggered(RuntimeEffects& runtime) const -> value::Bit {
  return value::Bit::FromBool(
      last_triggered_at_.has_value() && *last_triggered_at_ == runtime.Now());
}

}  // namespace lyra::runtime
