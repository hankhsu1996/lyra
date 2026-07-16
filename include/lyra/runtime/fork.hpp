#pragma once

#include <coroutine>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

// Shared join state for one fork. Each spawned branch (through its promise's
// completion callback) and the parent's JoinAwaitable hold a shared_ptr to it,
// so it frees itself once the last branch frame and the awaitable are gone.
// Branch completion is reported here, never to the engine: the engine only ever
// sees another coroutine to schedule. `completions_needed` is supplied by the
// caller -- the branch count for `join` (resume after the last), one for
// `join_any` (resume after the first).
class ForkGroup {
 public:
  ForkGroup(RuntimeServices& services, std::int64_t completions_needed)
      : services_(&services), completions_needed_(completions_needed) {
  }

  // True iff there is still an outstanding completion the parent should wait
  // for. A zero-branch fork or a `join_any` that satisfied synchronously
  // (which the engine's snapshot-drain forbids, but the guard remains as a
  // correctness anchor) needs no park.
  [[nodiscard]] auto NeedsPark() const -> bool {
    return completions_needed_ > 0;
  }

  void ParkParent(CoroutineHandle parent) {
    parent->Park(parked_parent_);
  }

  // Called from a branch's completion. Decrements the outstanding count and
  // wakes the parent once the threshold is reached.
  void OnBranchDone() {
    if (completions_needed_ > 0) {
      completions_needed_ -= 1;
    }
    if (completions_needed_ == 0) {
      if (Registration* parent = parked_parent_.PopFront()) {
        services_->ScheduleNextDelta(parent->activation);
      }
    }
  }

 private:
  RuntimeServices* services_;
  std::int64_t completions_needed_;
  // The join condition holds at most one activation: the process that executed
  // the fork.
  RegistrationList parked_parent_;
};

// What the parent `co_await`s after the branches are spawned. The wait reports
// ready iff every needed completion already arrived (which a zero-branch fork
// or a `join_any` with an immediate finisher can produce); otherwise the parent
// parks on the group.
class JoinAwaitable : public PendingWait {
 public:
  explicit JoinAwaitable(std::shared_ptr<ForkGroup> group)
      : group_(std::move(group)) {
  }

  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return !group_->NeedsPark();
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> parent) {
    CoroutineHandle token = &parent.promise();
    group_->ParkParent(token);
    token->process->BlockLeaf(token, this);
  }

  void await_resume() const noexcept {
  }

  // A join condition is monotonic (LRM 9.3.2): branch completions accumulate
  // during suspension. On resume, if the threshold is now met the parent is
  // runnable; otherwise re-park on the group. No engine services are needed.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeServices&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    if (!group_->NeedsPark()) {
      return PendingWaitOutcome::kRunnable;
    }
    group_->ParkParent(activation);
    return PendingWaitOutcome::kReblocked;
  }

 private:
  std::shared_ptr<ForkGroup> group_;
};

namespace detail {

inline void SpawnEach(
    RuntimeServices& services, std::shared_ptr<ForkGroup> group,
    std::vector<Coroutine<void>> branches) {
  for (auto& branch : branches) {
    branch.Handle().promise().on_complete = [group] { group->OnBranchDone(); };
    services.Spawn(std::move(branch));
  }
}

template <class... Branches>
auto Collect(Branches&&... branches) -> std::vector<Coroutine<void>> {
  std::vector<Coroutine<void>> out;
  out.reserve(sizeof...(Branches));
  (out.push_back(std::forward<Branches>(branches)), ...);
  return out;
}

}  // namespace detail

// LRM 9.3.2 Table 9-1 dispatch. `ForkWaitAll` (`join`) resumes the parent
// after every branch finishes; `ForkWaitFirst` (`join_any`) after the first;
// `SpawnAll` (`join_none`) returns void so the parent never waits at all.
// Branch ordering falls out of the engine's snapshot-drain -- a branch
// enqueued while the parent runs is reached only on the next drain pass,
// after the parent has parked (for `ForkWaitAll` / `ForkWaitFirst`) or moved
// on (for `SpawnAll`).
template <class... Branches>
auto ForkWaitAll(RuntimeServices& services, Branches... branches)
    -> JoinAwaitable {
  auto group = std::make_shared<ForkGroup>(
      services, static_cast<std::int64_t>(sizeof...(Branches)));
  detail::SpawnEach(services, group, detail::Collect(std::move(branches)...));
  return JoinAwaitable{group};
}

template <class... Branches>
auto ForkWaitFirst(RuntimeServices& services, Branches... branches)
    -> JoinAwaitable {
  auto group =
      std::make_shared<ForkGroup>(services, sizeof...(Branches) == 0 ? 0 : 1);
  detail::SpawnEach(services, group, detail::Collect(std::move(branches)...));
  return JoinAwaitable{group};
}

template <class... Branches>
void SpawnAll(RuntimeServices& services, Branches... branches) {
  (services.Spawn(std::move(branches)), ...);
}

// LRM 9.6.1 `wait fork`: block the executing process until every immediate
// child it spawned has terminated. The condition is read from the executing
// process; the frame parked on it is the one that ran `wait fork` (the task
// frame when `wait fork` sits in a task), so it is armed through the suspending
// handle rather than the process's own body.
class WaitForkAwaitable : public PendingWait {
 public:
  explicit WaitForkAwaitable(RuntimeServices& services) : services_(&services) {
  }

  [[nodiscard]] auto await_ready() const -> bool {
    return services_->CurrentProcess().HasNoLiveChild();
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> waiter) {
    CoroutineHandle token = &waiter.promise();
    services_->CurrentProcess().ArmWaitFork(token);
    token->process->BlockLeaf(token, this);
  }

  void await_resume() const noexcept {
  }

  // `wait fork` waits on the executing process's own immediate children (LRM
  // 9.6.1), a monotonic condition. On resume, if every immediate child has
  // terminated the process is runnable; otherwise re-park on its own condition.
  // The target is the process owning the waiting frame, not the resumer's
  // current process, so it is read from the activation.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeServices&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    RuntimeProcess& process = activation->Process();
    if (process.HasNoLiveChild()) {
      return PendingWaitOutcome::kRunnable;
    }
    process.ArmWaitFork(activation);
    return PendingWaitOutcome::kReblocked;
  }

 private:
  RuntimeServices* services_;
};

inline auto WaitFork(RuntimeServices& services) -> WaitForkAwaitable {
  return WaitForkAwaitable{services};
}

// LRM 9.6.3 `disable fork`: terminate every descendant of the executing
// process. The caller does not block -- the next statement runs at the same
// simulation time -- so this is a plain call rather than an awaitable. Like
// `wait fork`, it reads the executing process (LRM 9.5), so a `disable fork`
// inside a task reaches the descendants the enclosing process owns.
inline void DisableFork(RuntimeServices& services) {
  std::vector<CoroutineHandle> woken;
  services.CurrentProcess().DisableDescendants(woken);
  for (CoroutineHandle waiter : woken) {
    services.ScheduleNextDelta(waiter);
  }
}

}  // namespace lyra::runtime
