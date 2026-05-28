#include "lyra/runtime/runtime_process.hpp"

#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_scope.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(
    RuntimeScope& owner, ProcessKind kind, ProcessCoroutine coroutine)
    : owner_(&owner), kind_(kind), coroutine_(std::move(coroutine)) {
  // Wire the promise's back-pointer so coroutine-side code (awaitables)
  // can recover the RuntimeProcess identity from within await_suspend.
  coroutine_.BindProcess(*this);
}

auto RuntimeProcess::Owner() -> RuntimeScope& {
  return *owner_;
}

auto RuntimeProcess::Kind() const -> ProcessKind {
  return kind_;
}

auto RuntimeProcess::Resume() -> bool {
  if (state_ == ProcessState::kCompleted) {
    throw InternalError(
        "RuntimeProcess::Resume: cannot resume completed process");
  }
  if (state_ == ProcessState::kRunning) {
    throw InternalError("RuntimeProcess::Resume: reentrant resume");
  }
  state_ = ProcessState::kRunning;
  const bool completed = coroutine_.Resume();
  state_ = completed ? ProcessState::kCompleted : ProcessState::kWaiting;
  return completed;
}

}  // namespace lyra::runtime
