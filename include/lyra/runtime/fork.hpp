#pragma once

#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
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
    parent_ = parent;
    parent_parked_ = true;
  }

  // Called from a branch's completion. Decrements the outstanding count and
  // wakes the parent once the threshold is reached.
  void OnBranchDone() {
    if (completions_needed_ > 0) {
      completions_needed_ -= 1;
    }
    if (parent_parked_ && completions_needed_ == 0) {
      parent_parked_ = false;
      services_->ScheduleNextDelta(parent_);
    }
  }

 private:
  RuntimeServices* services_;
  std::int64_t completions_needed_;
  CoroutineHandle parent_{};
  bool parent_parked_ = false;
};

// What the parent `co_await`s after the branches are spawned. The wait reports
// ready iff every needed completion already arrived (which a zero-branch fork
// or a `join_any` with an immediate finisher can produce); otherwise the parent
// parks on the group.
class JoinAwaitable {
 public:
  explicit JoinAwaitable(std::shared_ptr<ForkGroup> group)
      : group_(std::move(group)) {
  }

  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return !group_->NeedsPark();
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> parent) noexcept {
    group_->ParkParent(&parent.promise());
  }

  void await_resume() const noexcept {
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

}  // namespace lyra::runtime
