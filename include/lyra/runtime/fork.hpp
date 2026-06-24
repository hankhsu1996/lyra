#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

// LRM 9.3.2 Table 9-1: which join keyword controls when the forking (parent)
// process resumes.
enum class JoinMode : std::uint8_t {
  kAll,   // resume after all branches finish (`join`)
  kAny,   // resume after the first branch finishes (`join_any`)
  kNone,  // resume immediately, branches run concurrently (`join_none`)
};

// Shared join state for one fork. Each spawned branch (through its promise's
// completion callback) and the parent's JoinAwaitable hold a shared_ptr to it,
// so it frees itself once the last branch frame and the awaitable are gone.
// Branch completion is reported here, never to the engine: the engine only ever
// sees another coroutine to schedule.
class ForkGroup {
 public:
  ForkGroup(RuntimeServices& services, std::size_t branch_count, JoinMode mode)
      : services_(&services),
        completions_needed_(NeededFor(branch_count, mode)) {
  }

  // join / join_any park the parent; join_none needs zero completions and so
  // never parks.
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
  static auto NeededFor(std::size_t branch_count, JoinMode mode)
      -> std::int64_t {
    switch (mode) {
      case JoinMode::kAll:
        return static_cast<std::int64_t>(branch_count);
      case JoinMode::kAny:
        return branch_count == 0 ? 0 : 1;
      case JoinMode::kNone:
        return 0;
    }
    return 0;
  }

  RuntimeServices* services_;
  std::int64_t completions_needed_;
  CoroutineHandle parent_{};
  bool parent_parked_ = false;
};

// What the parent `co_await`s after the branches are spawned. join_none reports
// ready and runs straight through; the others park the parent on the group.
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

// Spawns each branch as its own process and returns the parent's join wait.
// LRM 9.3.2: branches do not run until the parent blocks at the join (or, for
// join_none, continues past it) -- that ordering falls out of the engine's
// snapshot-drain, since a branch enqueued while the parent runs is reached only
// on the next drain pass, after the parent has parked or moved on.
inline auto Fork(
    RuntimeServices& services, std::vector<Coroutine<void>> branches,
    JoinMode mode) -> JoinAwaitable {
  auto group = std::make_shared<ForkGroup>(services, branches.size(), mode);
  for (auto& branch : branches) {
    branch.Handle().promise().on_complete = [group] { group->OnBranchDone(); };
    services.Spawn(std::move(branch));
  }
  return JoinAwaitable{group};
}

}  // namespace lyra::runtime
