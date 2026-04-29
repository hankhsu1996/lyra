#include "lyra/runtime/runtime_process.hpp"

#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(
    RuntimeScope& owner, ProcessKind kind, ProcessCoroutine coroutine)
    : owner_(&owner), kind_(kind), coroutine_(std::move(coroutine)) {
}

auto RuntimeProcess::Owner() -> RuntimeScope& {
  return *owner_;
}

auto RuntimeProcess::Kind() const -> ProcessKind {
  return kind_;
}

auto RuntimeProcess::Resume() -> ProcessRunResult {
  if (state_ == ProcessState::kCompleted) {
    throw InternalError(
        "RuntimeProcess::Resume: cannot resume completed process");
  }
  if (state_ == ProcessState::kRunning) {
    throw InternalError("RuntimeProcess::Resume: reentrant resume");
  }
  state_ = ProcessState::kRunning;
  auto result = coroutine_.Resume();
  if (result.IsCompleted()) {
    state_ = ProcessState::kCompleted;
    return result;
  }
  state_ = ProcessState::kWaiting;
  return result;
}

}  // namespace lyra::runtime
