#include "lyra/runtime/runtime_process.hpp"

#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/process_kind.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(ProcessKind kind, Coroutine coroutine)
    : kind_(kind), coroutine_(std::move(coroutine)) {
  // Wire the promise's back-pointer so coroutine-side code (awaitables)
  // can recover the RuntimeProcess identity from within await_suspend.
  coroutine_.BindProcess(*this);
}

auto RuntimeProcess::Kind() const -> ProcessKind {
  return kind_;
}

auto RuntimeProcess::ResumeWith(CoroutineHandle handle) -> bool {
  if (state_ == ProcessState::kCompleted) {
    throw InternalError(
        "RuntimeProcess::ResumeWith: cannot resume completed process");
  }
  if (state_ == ProcessState::kRunning) {
    throw InternalError("RuntimeProcess::ResumeWith: reentrant resume");
  }
  state_ = ProcessState::kRunning;
  handle.resume();
  // A task's exception propagates up the enable chain to the top-level promise,
  // so it is read there regardless of which frame threw.
  if (auto exc = std::exchange(
          coroutine_.Handle().promise().pending_exception, nullptr)) {
    std::rethrow_exception(exc);
  }
  const bool completed = coroutine_.Done();
  state_ = completed ? ProcessState::kCompleted : ProcessState::kWaiting;
  return completed;
}

}  // namespace lyra::runtime
