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
  watched.observable->Enrol(watched.part, membership);
}

Awaiter::Awaiter(std::size_t places) : memberships_(places) {
}

Awaiter::~Awaiter() = default;

void Awaiter::EnrolOnLeaf(const Trigger& trigger) {
  EnrolOnLeafAs(this, trigger);
}

void Awaiter::EnrolOnLeafAs(Observer observer, const Trigger& trigger) {
  EnrolOnPlace(NextMembership(), observer, trigger);
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
  return services.ParkRunning(wait->Awaited());
}

void ConsumeWait(Activation* activation) {
  activation->Withdraw();
  // Whatever holds an awaiter keeps it while its activation is parked there --
  // a frame cannot leave its scope then, and a reaction holds its wait for the
  // run -- so it is still there to ask; an activation already made runnable is
  // parked on none.
  if (activation->awaiter != nullptr &&
      activation->awaiter->IsReportFlushPoint()) {
    // Only a process keeps deferred reports, and a reaction has none until the
    // language asks for its process.
    if (RuntimeProcess* process = activation->ProcessIfMade()) {
      process->FlushDeferredReports();
    }
  }
  activation->awaiter = nullptr;
}

}  // namespace lyra::runtime
