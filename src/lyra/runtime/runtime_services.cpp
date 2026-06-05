#include "lyra/runtime/runtime_services.hpp"

#include <cstdint>
#include <functional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

void RuntimeServices::SubmitNba(std::function<void()> closure) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::SubmitNba: no Engine bound");
  }
  engine_->SubmitNba(std::move(closure));
}

void RuntimeServices::SubmitPostponed(std::function<void()> closure) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::SubmitPostponed: no Engine bound");
  }
  engine_->SubmitPostponed(std::move(closure));
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

void RuntimeServices::Spawn(Coroutine coroutine) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::Spawn: no Engine bound");
  }
  engine_->Spawn(std::move(coroutine));
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

auto RuntimeServices::TimeFormat() const -> const value::TimeFormat& {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::TimeFormat: no Engine bound");
  }
  return engine_->TimeFormat();
}

void RuntimeServices::SetTimeFormat(
    const value::PackedArray& units_power, const value::PackedArray& precision,
    const value::String& suffix, const value::PackedArray& min_width) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::SetTimeFormat: no Engine bound");
  }
  engine_->SetTimeFormat(
      static_cast<std::int8_t>(units_power.ToInt64()),
      static_cast<std::int32_t>(precision.ToInt64()),
      std::string(suffix.View()),
      static_cast<std::int32_t>(min_width.ToInt64()));
}

void RuntimeServices::ResetTimeFormat() {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ResetTimeFormat: no Engine bound");
  }
  engine_->ResetTimeFormat();
}

}  // namespace lyra::runtime
