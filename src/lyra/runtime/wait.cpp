#include "lyra/runtime/wait.hpp"

#include <cstddef>
#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

void EnrolOnPlace(
    WaitMembership& membership, Observer observer, const Trigger& watched) {
  // Storage that belongs to nothing is never told of a write, so there is
  // nothing to enrol on.
  if (watched.observable == nullptr) {
    return;
  }
  membership.observer = observer;
  membership.through = watched.through;
  membership.reads = watched.reads;
  membership.observation = watched.observation;
  watched.observable->MembersAt(watched.part).PushBack(membership);
}

Awaiter::Awaiter(std::size_t places) : memberships_(places) {
}

Awaiter::~Awaiter() = default;

void Awaiter::EnrolOnLeaf(const Trigger& trigger) {
  EnrolOnPlace(NextMembership(), this, trigger);
}

void Awaiter::EnrolOn(IntrusiveList<WaitMembership>& target) {
  WaitMembership& membership = NextMembership();
  membership.observer = this;
  target.PushBack(membership);
}

auto Awaiter::NextMembership() -> WaitMembership& {
  if (enrolled_ == memberships_.size()) {
    throw InternalError(
        "Awaiter: an awaiter enrolled on more places than it was built to "
        "watch");
  }
  return memberships_[enrolled_++];
}

auto StateAwaiter::Begin() -> Resumption {
  if (Holds()) {
    return WithoutStopping{};
  }
  return OnAnOccurrence{};
}

Wait::Wait() = default;

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
