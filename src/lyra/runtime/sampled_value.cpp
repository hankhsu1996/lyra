#include "lyra/runtime/sampled_value.hpp"

#include "lyra/base/time.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

SampledValue::SampledValue(const Trigger& watched) {
  membership_.observer = this;
  membership_.through = watched.through;
  membership_.reads = watched.reads;
  watched.observable->MembersAt(watched.part).PushBack(membership_);
}

SampledValue::~SampledValue() = default;

void SampledValue::Reached() {
  RuntimeEffects& runtime = current_runtime();
  const SimTime now = runtime.Now();
  if (submitted_for_ == now) {
    return;
  }
  submitted_for_ = now;
  runtime.Submit(runtime.Now(), Region::kPostponed, [this] { Take(); });
}

}  // namespace lyra::runtime
