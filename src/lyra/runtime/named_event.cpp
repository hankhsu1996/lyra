#include "lyra/runtime/named_event.hpp"

#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

NamedEvent::NamedEvent() = default;
NamedEvent::~NamedEvent() = default;

void NamedEvent::Trigger(RuntimeEffects& runtime) {
  last_triggered_at_ = runtime.Now();
  runtime.WakeWaitersOf(*this, MakeWholeValueProjectionTest());
}

auto NamedEvent::Triggered(RuntimeEffects& runtime) const
    -> value::PackedArray {
  const bool hit =
      last_triggered_at_.has_value() && *last_triggered_at_ == runtime.Now();
  return value::PackedArray::FromInt(hit ? 1 : 0, 1, false, false);
}

}  // namespace lyra::runtime
