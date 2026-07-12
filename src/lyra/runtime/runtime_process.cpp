#include "lyra/runtime/runtime_process.hpp"

#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/execution_context.hpp"
#include "lyra/runtime/process_kind.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(ProcessKind kind, Coroutine<void> coroutine)
    : kind_(kind), coroutine_(std::move(coroutine)) {
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
  wait_fork_registration_.Arm(waiter);
}

auto RuntimeProcess::HasNoLiveChild() const -> bool {
  for (const auto& child : children_) {
    if (child->execution_state_ != ProcessExecutionState::kTerminated) {
      return false;
    }
  }
  return true;
}

auto RuntimeProcess::TakeWaitForkWaiterIfSatisfied() -> CoroutineHandle {
  if (!HasNoLiveChild()) {
    return nullptr;
  }
  return wait_fork_registration_.TakeForWake();
}

auto RuntimeProcess::IsReleasable() const -> bool {
  return execution_state_ == ProcessExecutionState::kTerminated &&
         children_.empty();
}

void RuntimeProcess::AdoptChild(std::unique_ptr<RuntimeProcess> child) {
  child->parent_ = this;
  children_.push_back(std::move(child));
}

void RuntimeProcess::EraseChild(RuntimeProcess& child) {
  const std::size_t erased = std::erase_if(
      children_, [&](const std::unique_ptr<RuntimeProcess>& node) {
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

void RuntimeProcess::SettleTerminated() {
  execution_state_ = ProcessExecutionState::kTerminated;
  // The frame is parked at its final suspend point and holds the only copies of
  // this activation's automatic storage, so it is released with the terminal
  // state rather than pinned for as long as the node lives. A branch this body
  // spawned may still be running, which is why the node itself stays (LRM
  // 9.6.3).
  coroutine_ = Coroutine<void>{};
}

auto RuntimeProcess::ResumeWith(
    ExecutionContext& context, CoroutineHandle handle) -> bool {
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
  // A task's exception propagates up the enable chain to the top-level promise,
  // so it is read there regardless of which frame threw.
  if (auto exc = std::exchange(
          coroutine_.Handle().promise().pending_exception, nullptr)) {
    std::rethrow_exception(exc);
  }
  if (!coroutine_.Done()) {
    execution_state_ = ProcessExecutionState::kWaiting;
    return false;
  }
  SettleTerminated();
  return true;
}

}  // namespace lyra::runtime
