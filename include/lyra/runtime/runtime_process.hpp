#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/wake_registration.hpp"

namespace lyra::runtime {

class ExecutionContext;

enum class ProcessExecutionState : std::uint8_t {
  kCreated,
  kRunning,
  kWaiting,
  kTerminated,
};

// A node of the dynamic process lineage (LRM 9.5) and, while its body runs, the
// owner of the coroutine frame executing it. The two lifetimes are distinct:
// the frame is released the moment the body terminates, whereas the node
// survives for as long as any descendant is still alive. LRM 9.6.3 demands the
// separation -- `disable fork` terminates the descendants of subprocesses that
// have already terminated, so a terminated process stays reachable from its
// parent. The child list consequently holds terminated nodes alongside running
// ones.
//
// A process's children are those it spawned itself, whether directly or from a
// task or function it called (LRM 9.6.1): a subroutine call runs in the
// caller's thread and creates no process of its own. The list therefore
// accumulates across every fork the process executes.
class RuntimeProcess {
 public:
  RuntimeProcess(ProcessKind kind, Coroutine<void> coroutine);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  // Non-movable: the coroutine promise stores a back-pointer to this
  // RuntimeProcess, and so does every child node, so its address must stay
  // stable for as long as either can name it. Owners hold processes via
  // unique_ptr.
  RuntimeProcess(RuntimeProcess&&) noexcept = delete;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = delete;
  ~RuntimeProcess() = default;

  [[nodiscard]] auto Kind() const -> ProcessKind;

  // Resumes `handle` -- the specific coroutine frame that suspended (the
  // innermost one when a task enabled by this process is suspended). Symmetric
  // transfer carries control back up the enable chain. Returns true if the
  // whole process ran to completion (judged on the top-level coroutine), false
  // if it suspended again on some awaitable. Captured `handle` may be destroyed
  // by the time this returns, so completion and exceptions are read off the
  // top-level coroutine, not `handle`. Completing releases the frame; the node
  // outlives it.
  //
  // Taking the context is what makes this the only way a body can run: the
  // resumed body reaches its own process identity through the context, and a
  // caller cannot resume without supplying one.
  auto ResumeWith(ExecutionContext& context, CoroutineHandle handle) -> bool;

  // The top-level coroutine frame. Awaitables register the innermost handle for
  // wakeup; this is what the engine schedules to start the process and what
  // completion is judged against. Null once the body has terminated.
  [[nodiscard]] auto TopHandle() const -> CoroutineHandle;

  // Takes `child` into this process's lineage. A fork's parallel statement is a
  // thread of the process that executed the fork (LRM 9.5), which is the
  // process running when the branch is spawned, not the frame that spawned it.
  void AdoptChild(std::unique_ptr<RuntimeProcess> child);

  [[nodiscard]] auto Parent() const -> RuntimeProcess*;

  // Parks `waiter` on this process's `wait fork` condition (LRM 9.6.1). The
  // waiter is the frame that executed `wait fork` -- in a task it is the task
  // frame, not this process's own body -- so process identity and the parked
  // activation are deliberately distinct.
  void ArmWaitFork(CoroutineHandle waiter);

  // True when no immediate child is still executing: the whole live set has
  // reached a terminal state, or there were none. Terminated children retained
  // for their own live descendants (LRM 9.6.3) do not count as live. This is
  // the `wait fork` condition.
  [[nodiscard]] auto HasNoLiveChild() const -> bool;

  // If a frame is parked here on `wait fork` and the condition now holds,
  // unlink and return that frame for scheduling; null otherwise.
  [[nodiscard]] auto TakeWaitForkWaiterIfSatisfied() -> CoroutineHandle;

  // Releases `process`, whose body has just terminated, along with every
  // ancestor the release leaves with no lineage to retain, walking upward from
  // the leaf. Deliberately not a call on the released node: the release
  // destroys it. A process with no parent is owned by its scope and is never
  // released here.
  static void ReleaseTerminatedLineage(RuntimeProcess& process);

 private:
  // The sole writer of the terminal state, so a body can never be marked
  // terminated while it still owns the frame it ran in.
  void SettleTerminated();
  [[nodiscard]] auto IsReleasable() const -> bool;
  void EraseChild(RuntimeProcess& child);

  ProcessKind kind_;
  Coroutine<void> coroutine_;
  ProcessExecutionState execution_state_ = ProcessExecutionState::kCreated;
  RuntimeProcess* parent_ = nullptr;
  std::vector<std::unique_ptr<RuntimeProcess>> children_;
  WakeRegistration wait_fork_registration_;
};

}  // namespace lyra::runtime
