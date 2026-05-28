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
    Observable& observable, EdgeTransition transition) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::TriggerValueChange: no Engine bound");
  }
  engine_->TriggerValueChange(observable, transition);
}

void RuntimeServices::ScheduleProcess(RuntimeProcess& process) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ScheduleProcess: no Engine bound");
  }
  engine_->ScheduleProcess(process);
}

void RuntimeServices::ScheduleInactive(RuntimeProcess& process) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ScheduleInactive: no Engine bound");
  }
  engine_->ScheduleInactiveProcess(process);
}

void RuntimeServices::ScheduleAtTime(SimTime when, RuntimeProcess& process) {
  if (engine_ == nullptr) {
    throw InternalError("RuntimeServices::ScheduleAtTime: no Engine bound");
  }
  engine_->ScheduleAtTime(when, process);
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

}  // namespace lyra::runtime
