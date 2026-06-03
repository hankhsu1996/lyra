#include "lyra/runtime/runtime_services.hpp"

#include <functional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

void RuntimeServices::SubmitNba(std::function<void()> closure) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::SubmitNba: no Engine bound");
  }
  engine_->SubmitNba(std::move(closure));
}

void RuntimeServices::TriggerValueChange(
    Observable& observable, const EdgeClassifier& classify) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::TriggerValueChange: no Engine bound");
  }
  engine_->TriggerValueChange(observable, classify);
}

void RuntimeServices::ScheduleNextDelta(CoroutineHandle handle) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ScheduleNextDelta: no Engine bound");
  }
  engine_->ScheduleNextDelta(handle);
}

void RuntimeServices::ScheduleInactive(CoroutineHandle handle) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ScheduleInactive: no Engine bound");
  }
  engine_->ScheduleInactive(handle);
}

void RuntimeServices::ScheduleAtTime(SimTime when, CoroutineHandle handle) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ScheduleAtTime: no Engine bound");
  }
  engine_->ScheduleAtTime(when, handle);
}

void RuntimeServices::RequestFinish(int level) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::RequestFinish: no Engine bound");
  }
  engine_->RequestFinish(level);
}

auto RuntimeServices::Now() const -> SimTime {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::Now: no Engine bound");
  }
  return engine_->Now();
}

auto RuntimeServices::GlobalPrecisionPower() const -> std::int8_t {
  if (engine_ == nullptr) {
    throw InternalError(
        "RuntimeServices::GlobalPrecisionPower: no Engine bound");
  }
  return engine_->GlobalPrecisionPower();
}

}  // namespace lyra::runtime
