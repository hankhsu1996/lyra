#include "lyra/runtime/fork.hpp"

#include <algorithm>
#include <cstddef>
#include <memory>
#include <span>
#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

// Waiting for a fork's branches to terminate (LRM 9.3.2): every one of them
// for `join`, the first for `join_any`. A branch that is killed terminates as
// surely as one that runs out its statements, so the wait watches each
// branch's termination, as an `await` of it would (LRM 9.7). A zero-branch
// fork's condition holds before anything terminates.
class JoinAwaiter final : public StateAwaiter {
 public:
  JoinAwaiter(
      std::vector<std::shared_ptr<RuntimeProcess>> branches,
      std::size_t terminations_needed)
      : branches_(std::move(branches)),
        terminations_needed_(terminations_needed) {
    for (const std::shared_ptr<RuntimeProcess>& branch : branches_) {
      EnrolOn(branch->Termination());
    }
  }

  // Rejoining branches is neither an event control nor a wait statement, so
  // LRM 12.4.2.1 does not make it a flush point: reports the parent raised
  // before the fork stay pending across the join.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  [[nodiscard]] auto Holds() const -> bool override {
    const auto terminated = std::ranges::count_if(
        branches_, [](const std::shared_ptr<RuntimeProcess>& branch) {
          return branch->ExecutionState() == ProcessExecutionState::kTerminated;
        });
    return static_cast<std::size_t>(terminated) >= terminations_needed_;
  }

  std::vector<std::shared_ptr<RuntimeProcess>> branches_;
  std::size_t terminations_needed_;
};

// LRM 9.6.1 `wait fork`. The condition belongs to the process the waiting frame
// runs in, which is the process running where the wait is built -- and stays
// its own on a restart, where another process is the one running.
class WaitForkAwaiter final : public StateAwaiter {
 public:
  explicit WaitForkAwaiter(RuntimeProcess& process) : process_(&process) {
    EnrolOn(process.WaitForkCondition());
  }

  // `wait fork` is a wait statement (LRM 9.6.1), one of the two forms
  // LRM 12.4.2.1 makes a violation report flush point.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 private:
  [[nodiscard]] auto Holds() const -> bool override {
    return process_->HasNoLiveChild();
  }

  RuntimeProcess* process_;
};

// Hands every branch to the engine, answering the processes they now are.
auto SpawnBranches(RuntimeEffects& runtime, std::span<Coroutine<void>> branches)
    -> std::vector<std::shared_ptr<RuntimeProcess>> {
  std::vector<std::shared_ptr<RuntimeProcess>> spawned;
  spawned.reserve(branches.size());
  for (Coroutine<void>& branch : branches) {
    spawned.push_back(runtime.Spawn(std::move(branch)));
  }
  return spawned;
}

}  // namespace

auto ForkWaitAll(RuntimeEffects& runtime, std::span<Coroutine<void>> branches)
    -> Wait {
  return MakeWait<JoinAwaiter>(
      SpawnBranches(runtime, branches), branches.size());
}

auto ForkWaitFirst(RuntimeEffects& runtime, std::span<Coroutine<void>> branches)
    -> Wait {
  return MakeWait<JoinAwaiter>(
      SpawnBranches(runtime, branches),
      std::min<std::size_t>(branches.size(), 1));
}

void SpawnAll(RuntimeEffects& runtime, std::span<Coroutine<void>> branches) {
  for (Coroutine<void>& branch : branches) {
    runtime.Spawn(std::move(branch));
  }
}

auto WaitFork(RuntimeEffects& runtime) -> Wait {
  return MakeWait<WaitForkAwaiter>(runtime.CurrentProcess());
}

void DisableFork(RuntimeEffects& runtime) {
  runtime.CurrentProcess().DisableDescendants(runtime);
}

}  // namespace lyra::runtime
