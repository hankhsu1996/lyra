#include "lyra/runtime/sampled_value.hpp"

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

SampledValue::SampledValue(const Trigger& watched) {
  EnrolOnPlace(membership_, this, watched);
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
