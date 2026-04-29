#include "lyra/runtime/runtime_process.hpp"

#include <utility>

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
  return coroutine_.Resume();
}

}  // namespace lyra::runtime
