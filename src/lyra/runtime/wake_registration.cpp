#include "lyra/runtime/wake_registration.hpp"

#include "lyra/runtime/coroutine.hpp"

namespace lyra::runtime {

WakeRegistration::~WakeRegistration() {
  if (waiter_ != nullptr) {
    waiter_->parked_registration = nullptr;
  }
}

void WakeRegistration::Arm(PromiseBase* waiter) {
  waiter_ = waiter;
  waiter->parked_registration = this;
}

auto WakeRegistration::TakeForWake() -> PromiseBase* {
  PromiseBase* waiter = waiter_;
  if (waiter != nullptr) {
    waiter->parked_registration = nullptr;
    waiter_ = nullptr;
  }
  return waiter;
}

void WakeRegistration::RevokeWaiter(PromiseBase* waiter) {
  if (waiter_ != waiter) {
    return;
  }
  waiter_ = nullptr;
  waiter->parked_registration = nullptr;
}

PromiseBase::~PromiseBase() {
  if (parked_registration != nullptr) {
    parked_registration->RevokeWaiter(this);
  }
}

}  // namespace lyra::runtime
