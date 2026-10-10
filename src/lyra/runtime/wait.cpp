#include "lyra/runtime/wait.hpp"

#include <memory>
#include <utility>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

Awaiter::~Awaiter() = default;

void Awaiter::EnrolOnLeaf(const Trigger& trigger) {
  // Storage that belongs to nothing is never told of a write, so a wait on it
  // has nothing to enrol on and waits on its other leaves.
  if (trigger.observable == nullptr) {
    return;
  }
  Enrol(
      trigger.observable->Members(), trigger.through, trigger.reads,
      trigger.observation);
}

void Awaiter::EnrolOn(IntrusiveList<WaitMembership>& target) {
  Enrol(target, nullptr, value::BitPositions{}, Observation{});
}

void Awaiter::Enrol(
    IntrusiveList<WaitMembership>& target, const ErasedReference* through,
    value::BitPositions reads, Observation observation) {
  WaitMembership& membership = memberships_.emplace_back();
  membership.awaiter = this;
  membership.through = through;
  membership.reads = reads;
  membership.observation = std::move(observation);
  target.PushBack(membership);
}

auto StateAwaiter::Begin() -> Resumption {
  if (Holds()) {
    return WithoutStopping{};
  }
  return OnAnOccurrence{};
}

Wait::Wait(std::unique_ptr<Awaiter> awaiter) : awaiter_(std::move(awaiter)) {
}

Wait::Wait(Wait&&) noexcept = default;
auto Wait::operator=(Wait&&) noexcept -> Wait& = default;
Wait::~Wait() = default;

auto ParkAt(RuntimeEffects& services, Wait* wait) -> bool {
  return services.CurrentProcess().ParkAt(services, wait->Awaited());
}

void ConsumeWait(Activation* activation) {
  activation->Withdraw();
  // An awaiter lives in the frame until the frame leaves its scope, which it
  // cannot do while parked, so it is still there to ask; an activation already
  // made runnable is parked on none.
  if (activation->awaiter != nullptr &&
      activation->awaiter->IsReportFlushPoint()) {
    activation->Process().FlushDeferredReports();
  }
  activation->awaiter = nullptr;
}

}  // namespace lyra::runtime
