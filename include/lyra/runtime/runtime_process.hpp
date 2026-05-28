#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"

namespace lyra::runtime {

class Observable;
class RuntimeScope;

enum class ProcessState : std::uint8_t {
  kCreated,
  kRunning,
  kWaiting,
  kCompleted,
};

class RuntimeProcess {
 public:
  RuntimeProcess(
      RuntimeScope& owner, ProcessKind kind, ProcessCoroutine coroutine);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  // Non-movable: the coroutine promise stores a back-pointer to this
  // RuntimeProcess, so its address must be stable for the lifetime of the
  // coroutine. Owners (RuntimeScope) hold processes via unique_ptr.
  RuntimeProcess(RuntimeProcess&&) noexcept = delete;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = delete;
  ~RuntimeProcess() = default;

  auto Owner() -> RuntimeScope&;
  [[nodiscard]] auto Kind() const -> ProcessKind;
  [[nodiscard]] auto State() const -> ProcessState {
    return state_;
  }
  // Returns true if the coroutine ran to completion this resume, false if it
  // suspended on some awaitable (the awaitable has arranged its own wakeup).
  auto Resume() -> bool;

  // Used by the engine for multi-trigger event-control waits: when the process
  // subscribes to N Observables, the engine stores them here; when any one
  // fires, the engine sweeps the rest to remove the dangling subscriptions.
  void SetPendingValueChangeSubscriptions(std::vector<Observable*> subs) {
    pending_value_change_subs_ = std::move(subs);
  }

  auto TakePendingValueChangeSubscriptions() -> std::vector<Observable*> {
    return std::exchange(pending_value_change_subs_, {});
  }

 private:
  RuntimeScope* owner_ = nullptr;
  ProcessKind kind_;
  ProcessCoroutine coroutine_;
  ProcessState state_ = ProcessState::kCreated;
  std::vector<Observable*> pending_value_change_subs_;
};

}  // namespace lyra::runtime
