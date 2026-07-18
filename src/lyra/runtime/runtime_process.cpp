#include "lyra/runtime/runtime_process.hpp"

#include <algorithm>
#include <cstddef>
#include <exception>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/execution_context.hpp"
#include "lyra/runtime/process_kind.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(ProcessKind kind, Coroutine<void> coroutine)
    : kind_(kind),
      coroutine_(std::move(coroutine)),
      // Before the body runs, the top frame is the active leaf (what the engine
      // schedules to start the process); a wait moves the leaf inward.
      current_leaf_(coroutine_.Token()) {
  // Wire the promise's back-pointer so coroutine-side code (awaitables)
  // can recover the RuntimeProcess identity from within await_suspend.
  coroutine_.BindProcess(*this);
}

auto RuntimeProcess::Kind() const -> ProcessKind {
  return kind_;
}

auto RuntimeProcess::TopHandle() const -> CoroutineHandle {
  return coroutine_.Token();
}

auto RuntimeProcess::Parent() const -> RuntimeProcess* {
  return parent_;
}

void RuntimeProcess::ArmWaitFork(CoroutineHandle waiter) {
  waiter->Park(parked_wait_fork_);
}

void RuntimeProcess::ArmTerminatedWaiter(CoroutineHandle waiter) {
  waiter->Park(terminated_waiters_);
}

void RuntimeProcess::Suspend() {
  if (execution_state_ == ProcessExecutionState::kSuspended ||
      execution_state_ == ProcessExecutionState::kTerminated) {
    return;
  }
  // Detach the leaf from whatever holds it -- a wait target (desensitize) or a
  // run queue (dequeue). Its pending wait, if any, is a separate member and
  // stays: it is the saved blocked disposition resume re-establishes.
  if (current_leaf_ != nullptr) {
    current_leaf_->RevokeRegistrations();
  }
  execution_state_ = ProcessExecutionState::kSuspended;
}

void RuntimeProcess::MarkResumed() {
  execution_state_ = ProcessExecutionState::kWaiting;
}

auto RuntimeProcess::HasNoLiveChild() const -> bool {
  return std::ranges::all_of(children_, [](const auto& child) {
    return child->execution_state_ == ProcessExecutionState::kTerminated;
  });
}

auto RuntimeProcess::TakeWaitForkWaiterIfSatisfied() -> CoroutineHandle {
  if (!HasNoLiveChild()) {
    return nullptr;
  }
  Registration* waiter = parked_wait_fork_.PopFront();
  return waiter != nullptr ? waiter->activation : nullptr;
}

void RuntimeProcess::DisableDescendants(std::vector<CoroutineHandle>& woken) {
  for (const std::shared_ptr<RuntimeProcess>& child : children_) {
    // Sever the upward link before the recursion severs the downward ones, so a
    // handle-held child left behind by the clear below is a parent-less orphan
    // rather than a node pointing into freed storage.
    child->parent_ = nullptr;
    child->TerminateSubtreeKilled(woken);
  }
  children_.clear();
}

void RuntimeProcess::TerminateSubtreeKilled(
    std::vector<CoroutineHandle>& woken) {
  DisableDescendants(woken);
  if (execution_state_ != ProcessExecutionState::kTerminated) {
    SettleTerminated(ProcessTerminationCause::kKilled, woken);
  }
}

void RuntimeProcess::TerminateSubtreeDeferringRunning(
    RuntimeProcess& running, std::vector<CoroutineHandle>& woken) {
  // Off-path children are killed and severed synchronously; the one child on
  // the path down to `running` is kept linked and recursed into, so the chain
  // that owns `running` survives until `running` settles at its safe boundary.
  std::erase_if(children_, [&](const std::shared_ptr<RuntimeProcess>& child) {
    if (child->IsSelfOrAncestorOf(running)) {
      child->TerminateSubtreeDeferringRunning(running, woken);
      return false;
    }
    child->parent_ = nullptr;
    child->TerminateSubtreeKilled(woken);
    return true;
  });
  if (this == &running) {
    RequestTermination(ProcessTerminationCause::kKilled);
    return;
  }
  if (execution_state_ != ProcessExecutionState::kTerminated) {
    SettleTerminated(ProcessTerminationCause::kKilled, woken);
  }
}

auto RuntimeProcess::IsSelfOrAncestorOf(const RuntimeProcess& other) const
    -> bool {
  for (const RuntimeProcess* node = &other; node != nullptr;
       node = node->parent_) {
    if (node == this) {
      return true;
    }
  }
  return false;
}

void RuntimeProcess::DetachFromParent() {
  if (parent_ == nullptr) {
    return;
  }
  RuntimeProcess* parent = parent_;
  parent_ = nullptr;
  parent->EraseChild(*this);
}

auto RuntimeProcess::IsReleasable() const -> bool {
  return execution_state_ == ProcessExecutionState::kTerminated &&
         children_.empty();
}

void RuntimeProcess::AdoptChild(std::shared_ptr<RuntimeProcess> child) {
  child->parent_ = this;
  children_.push_back(std::move(child));
}

void RuntimeProcess::EraseChild(RuntimeProcess& child) {
  const std::size_t erased = std::erase_if(
      children_, [&](const std::shared_ptr<RuntimeProcess>& node) {
        return node.get() == &child;
      });
  if (erased != 1) {
    throw InternalError("RuntimeProcess::EraseChild: child is not ours");
  }
}

void RuntimeProcess::ReleaseTerminatedLineage(RuntimeProcess& process) {
  RuntimeProcess* node = &process;
  while (node->parent_ != nullptr && node->IsReleasable()) {
    RuntimeProcess* parent = node->parent_;
    parent->EraseChild(*node);
    node = parent;
  }
}

void RuntimeProcess::RequestTermination(ProcessTerminationCause cause) {
  if (execution_state_ == ProcessExecutionState::kTerminated ||
      termination_requested_) {
    return;
  }
  termination_requested_ = true;
  termination_cause_ = cause;
  // Detach the leaf from whatever holds it -- a wait target or a run queue --
  // by explicit revoke. The frame is not destroyed here (it is still going to
  // unwind) to revoke its registrations implicitly, so revoking now is what
  // keeps a settled-later frame un-nameable in between.
  if (current_leaf_ != nullptr) {
    current_leaf_->RevokeRegistrations();
  }
}

void RuntimeProcess::SettleTerminated(
    ProcessTerminationCause cause, std::vector<CoroutineHandle>& woken) {
  execution_state_ = ProcessExecutionState::kTerminated;
  termination_cause_ = cause;
  // The frame is parked at its final suspend point (normal completion) or at
  // some blocking point (a kill), and holds the only copies of this
  // activation's automatic storage, so it is released with the terminal state
  // rather than pinned for as long as the node lives. Releasing it destroys the
  // frame, which revokes every registration it held -- so a killed process,
  // parked anywhere, is left unable to resume. A branch this body spawned may
  // still be running, which is why the node itself stays (LRM 9.6.3).
  coroutine_ = Coroutine<void>{};
  // Settling and draining the `await` waiters are one step: a process reaching
  // terminal always hands its waiters to `woken` in the same primitive, so no
  // terminal path can leave an awaiter parked forever (LRM 9.7).
  while (Registration* waiter = terminated_waiters_.PopFront()) {
    woken.push_back(waiter->activation);
  }
}

auto RuntimeProcess::ResumeWith(
    ExecutionContext& context, CoroutineHandle handle,
    std::vector<CoroutineHandle>& woken) -> bool {
  if (execution_state_ == ProcessExecutionState::kTerminated) {
    throw InternalError(
        "RuntimeProcess::ResumeWith: cannot resume terminated process");
  }
  if (execution_state_ == ProcessExecutionState::kRunning) {
    throw InternalError("RuntimeProcess::ResumeWith: reentrant resume");
  }
  execution_state_ = ProcessExecutionState::kRunning;
  {
    const ExecutionContextGuard guard(context, *this);
    handle->self.resume();
  }
  if (!coroutine_.Done()) {
    execution_state_ = ProcessExecutionState::kWaiting;
    return false;
  }
  // Phase 2 of a deferred termination: the body requested its own termination
  // (LRM 9.6 / 9.7) and has now unwound to this safe boundary. Under a pending
  // request the unwind must be the termination sentinel: consume it here, and
  // let any other fault raised while unwinding propagate rather than be
  // swallowed. Then publish the recorded cause atomically -- terminal state,
  // frame release, waiter drain.
  if (termination_requested_) {
    if (std::exception_ptr fault = coroutine_.Handle().promise().TakeFault()) {
      try {
        std::rethrow_exception(fault);
      } catch (const ProcessTerminationUnwind&) {
      }
    }
    SettleTerminated(termination_cause_, woken);
    return true;
  }
  // A task's exception has propagated up the enable chain into this top-level
  // frame regardless of which frame threw. Take the fault out before releasing
  // the frame so a faulted process reaches its terminal state and frees its
  // frame on the same path a successful one does, then re-raise it.
  std::exception_ptr fault = coroutine_.Handle().promise().TakeFault();
  SettleTerminated(ProcessTerminationCause::kCompleted, woken);
  if (fault) {
    std::rethrow_exception(fault);
  }
  return true;
}

}  // namespace lyra::runtime
