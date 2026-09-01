#pragma once

#include <array>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

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
  ForkGroup(RuntimeEffects& runtime, std::int64_t completions_needed)
      : runtime_(&runtime), completions_needed_(completions_needed) {
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
        runtime_->ScheduleNextDelta(parent->activation);
      }
    }
  }

 private:
  RuntimeEffects* runtime_;
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
    BlockOn(token);
  }

  void await_resume() const {
    CheckAbortOnResume();
  }

  // A join condition is monotonic (LRM 9.3.2): branch completions accumulate
  // during suspension. On resume, if the threshold is now met the parent is
  // runnable; otherwise re-park on the group. No runtime access is needed.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeEffects&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    if (!group_->NeedsPark()) {
      return PendingWaitOutcome::kRunnable;
    }
    group_->ParkParent(activation);
    return PendingWaitOutcome::kReblocked;
  }

  // Rejoining branches is neither an event control nor a wait statement, so
  // LRM 12.4.2.1 does not make it a flush point: reports the parent raised
  // before the fork stay pending across the join.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  std::shared_ptr<ForkGroup> group_;
};

namespace detail {

// Hands every branch to the engine with its completion reported to one join
// condition.
template <class Branches>
auto SpawnUnderGroup(
    RuntimeEffects& runtime, Branches& branches,
    std::int64_t completions_needed) -> std::shared_ptr<ForkGroup> {
  auto group = std::make_shared<ForkGroup>(runtime, completions_needed);
  for (auto& branch : branches) {
    branch.Handle().promise().on_complete = [group] { group->OnBranchDone(); };
    runtime.Spawn(std::move(branch));
  }
  return group;
}

// How many completions each join mode waits for (LRM 9.3.2 Table 9-1): every
// branch, or the first of however many there are.
constexpr auto CompletionsForAll(std::size_t branches) -> std::int64_t {
  return static_cast<std::int64_t>(branches);
}
constexpr auto CompletionsForFirst(std::size_t branches) -> std::int64_t {
  return branches == 0 ? 0 : 1;
}

// Spawns the branches under one join condition and parks the executing process
// on it, answering whether that process must suspend at all. A caller holding a
// frame the condition can live in awaits instead; this is for one whose body is
// generated code, where the suspension is a control edge and the answer has to
// cross as a value.
inline auto SpawnAndPark(
    RuntimeEffects& runtime, std::vector<Coroutine<void>> branches,
    std::int64_t completions_needed) -> bool {
  const std::shared_ptr<ForkGroup> group =
      SpawnUnderGroup(runtime, branches, completions_needed);
  if (!group->NeedsPark()) {
    return false;
  }
  runtime.CurrentProcess().RegisterWakeup(
      [&group](CoroutineHandle parent) { group->ParkParent(parent); });
  return true;
}

}  // namespace detail

// LRM 9.3.2 Table 9-1 dispatch. `ForkWaitAll` (`join`) resumes the parent
// after every branch finishes; `ForkWaitFirst` (`join_any`) after the first;
// `SpawnAll` (`join_none`) returns void so the parent never waits at all.
// Branch ordering falls out of the engine's snapshot-drain -- a branch
// enqueued while the parent runs is reached only on the next drain pass,
// after the parent has parked (for `ForkWaitAll` / `ForkWaitFirst`) or moved
// on (for `SpawnAll`).
template <std::size_t N>
auto ForkWaitAll(
    RuntimeEffects& runtime, std::array<Coroutine<void>, N> branches)
    -> JoinAwaitable {
  return JoinAwaitable{
      detail::SpawnUnderGroup(runtime, branches, detail::CompletionsForAll(N))};
}

template <std::size_t N>
auto ForkWaitFirst(
    RuntimeEffects& runtime, std::array<Coroutine<void>, N> branches)
    -> JoinAwaitable {
  return JoinAwaitable{detail::SpawnUnderGroup(
      runtime, branches, detail::CompletionsForFirst(N))};
}

template <std::size_t N>
void SpawnAll(
    RuntimeEffects& runtime, std::array<Coroutine<void>, N> branches) {
  for (auto& branch : branches) {
    runtime.Spawn(std::move(branch));
  }
}

// The two joins again, for a caller whose body is generated code: each answers
// whether the process must suspend, since the suspension there is a control
// edge the answer decides rather than an awaitable that decides for itself.
// `join_none` needs no such form, having nothing to wait for.
inline auto ForkWaitAllMustPark(
    RuntimeEffects& runtime, std::vector<Coroutine<void>> branches) -> bool {
  const std::int64_t needed = detail::CompletionsForAll(branches.size());
  return detail::SpawnAndPark(runtime, std::move(branches), needed);
}

inline auto ForkWaitFirstMustPark(
    RuntimeEffects& runtime, std::vector<Coroutine<void>> branches) -> bool {
  const std::int64_t needed = detail::CompletionsForFirst(branches.size());
  return detail::SpawnAndPark(runtime, std::move(branches), needed);
}

// LRM 9.6.1 `wait fork`: block the executing process until every immediate
// child it spawned has terminated. The condition is read from the executing
// process; the frame parked on it is the one that ran `wait fork` (the task
// frame when `wait fork` sits in a task), so it is armed through the suspending
// handle rather than the process's own body.
class WaitForkAwaitable : public PendingWait {
 public:
  explicit WaitForkAwaitable(RuntimeEffects& runtime) : runtime_(&runtime) {
  }

  [[nodiscard]] auto await_ready() const -> bool {
    return runtime_->CurrentProcess().HasNoLiveChild();
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> waiter) {
    CoroutineHandle token = &waiter.promise();
    runtime_->CurrentProcess().ArmWaitFork(token);
    BlockOn(token);
  }

  void await_resume() const {
    CheckAbortOnResume();
  }

  // `wait fork` waits on the executing process's own immediate children (LRM
  // 9.6.1), a monotonic condition. On resume, if every immediate child has
  // terminated the process is runnable; otherwise re-park on its own condition.
  // The target is the process owning the waiting frame, not the resumer's
  // current process, so it is read from the activation.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeEffects&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    RuntimeProcess& process = activation->Process();
    if (process.HasNoLiveChild()) {
      return PendingWaitOutcome::kRunnable;
    }
    process.ArmWaitFork(activation);
    return PendingWaitOutcome::kReblocked;
  }

  // `wait fork` is a wait statement (LRM 9.6.1), one of the two forms
  // LRM 12.4.2.1 makes a violation report flush point.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 private:
  RuntimeEffects* runtime_;
};

inline auto WaitFork(RuntimeEffects& runtime) -> WaitForkAwaitable {
  return WaitForkAwaitable{runtime};
}

// LRM 9.6.3 `disable fork`: terminate every descendant of the executing
// process. The caller does not block -- the next statement runs at the same
// simulation time -- so this is a plain call rather than an awaitable. Like
// `wait fork`, it reads the executing process (LRM 9.5), so a `disable fork`
// inside a task reaches the descendants the enclosing process owns.
inline void DisableFork(RuntimeEffects& runtime) {
  std::vector<CoroutineHandle> woken;
  runtime.CurrentProcess().DisableDescendants(woken);
  for (CoroutineHandle waiter : woken) {
    runtime.ScheduleNextDelta(waiter);
  }
}

}  // namespace lyra::runtime
