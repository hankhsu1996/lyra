#pragma once

#include <coroutine>
#include <functional>
#include <memory>
#include <utility>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

// The execution vehicle that carries an activation's thread across a suspension
// when the scheduler cannot re-enter the activation's coroutine directly: its
// continuation is buried under a native call stack the runtime does not own (a
// DPI foreign task, LRM 35.5.2). Resuming re-enters that native stack on its
// own stack and lets it run on to its next suspension or its return, so the
// scheduler drives this vehicle instead of the coroutine handle.
//
// A plain coroutine activation names no vehicle: the scheduler resumes its
// innermost frame with a symmetric transfer. Only an activation that blocked
// while running under a foreign call names one, snapshotted when it blocked.
class ForeignExecution {
 public:
  ForeignExecution() = default;
  ForeignExecution(const ForeignExecution&) = delete;
  auto operator=(const ForeignExecution&) -> ForeignExecution& = delete;
  ForeignExecution(ForeignExecution&&) = delete;
  auto operator=(ForeignExecution&&) -> ForeignExecution& = delete;
  virtual ~ForeignExecution() = default;

  // Re-enter the native stack this vehicle owns and run it on until it yields
  // again or returns. Returns to the caller either way; `IsDone` distinguishes
  // the two. The activation's coroutine is resumed from within that stack, not
  // by the caller.
  virtual void Resume() = 0;

  // Whether the foreign call has returned. False while it is suspended mid-call
  // with more to run; true once its entry has run to completion.
  [[nodiscard]] virtual auto IsDone() const -> bool = 0;
};

// Creates a foreign execution that runs `entry` -- the foreign call -- on its
// own stack. It does not start: the first `Resume` enters it. `entry` returning
// marks it done.
auto MakeForeignExecution(std::function<void()> entry)
    -> std::unique_ptr<ForeignExecution>;

// Suspends the foreign execution currently running on this thread, handing
// control back to whoever last resumed it. Called from within the foreign call
// (the exported-task driver when its SV body suspends across the boundary); the
// matching `Resume` continues just past here. It is an error to call this with
// no foreign execution running on the thread.
void YieldForeignExecution();

// The process whose foreign call is running on this thread right now -- the one
// that entered the current foreign execution. The exported-task driver, reached
// from foreign C with no engine handle, uses it to run the exported body as
// that process's thread (LRM 9.5). It is an error to call this outside a
// foreign call.
auto CurrentForeignProcess() -> RuntimeProcess&;

// Installs the current foreign call -- vehicle `fe` and its process `process`
// -- for the dynamic extent of a scope, the two facts an inner frame needs and
// cannot reach on its own. An activation that blocks inside this extent
// snapshots `fe` as its resume vehicle, so the scheduler later resumes it by
// driving `fe` rather than its coroutine handle; the exported-task driver,
// given no engine handle by foreign C, reads `process` to run its body as that
// process's thread. Both are saved and restored, so a nested foreign call
// unwinds to its caller's and a process outside any foreign call resumes its
// coroutine directly.
class ForeignExecutionGuard {
 public:
  ForeignExecutionGuard(RuntimeProcess& process, ForeignExecution& fe);
  ForeignExecutionGuard(const ForeignExecutionGuard&) = delete;
  auto operator=(const ForeignExecutionGuard&)
      -> ForeignExecutionGuard& = delete;
  ForeignExecutionGuard(ForeignExecutionGuard&&) = delete;
  auto operator=(ForeignExecutionGuard&&) -> ForeignExecutionGuard& = delete;
  ~ForeignExecutionGuard();

 private:
  RuntimeProcess* process_;
  ForeignExecution* previous_execution_;
  RuntimeProcess* previous_process_;
};

// The awaitable a DPI import task (LRM 35.5.2) awaits at its foreign-call step:
// it runs `foreign_call` on a fiber so an exported task the call reaches can
// suspend across the boundary. Awaiting it suspends the import frame only if
// the call actually suspends; a call that returns without consuming time
// completes within the await.
class ForeignTaskAwaitable {
 public:
  explicit ForeignTaskAwaitable(std::function<void()> foreign_call)
      : fiber_(MakeForeignExecution(std::move(foreign_call))) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  auto await_suspend(std::coroutine_handle<P> handle) -> bool {
    CoroutineHandle token = &handle.promise();
    return !token->process->EnterForeignExecution(token, *fiber_);
  }

  static void await_resume() noexcept {
  }

 private:
  std::unique_ptr<ForeignExecution> fiber_;
};

inline auto RunForeignTaskOnFiber(std::function<void()> foreign_call)
    -> ForeignTaskAwaitable {
  return ForeignTaskAwaitable{std::move(foreign_call)};
}

}  // namespace lyra::runtime
