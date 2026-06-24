#pragma once

#include <cstdint>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/process_kind.hpp"

namespace lyra::runtime {

enum class ProcessState : std::uint8_t {
  kCreated,
  kRunning,
  kWaiting,
  kCompleted,
};

class RuntimeProcess {
 public:
  RuntimeProcess(ProcessKind kind, Coroutine<void> coroutine);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  // Non-movable: the coroutine promise stores a back-pointer to this
  // RuntimeProcess, so its address must be stable for the lifetime of the
  // coroutine. Owners (RuntimeScope) hold processes via unique_ptr.
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
  // top-level coroutine, not `handle`.
  auto ResumeWith(CoroutineHandle handle) -> bool;

  // The top-level coroutine frame. Awaitables register the innermost handle for
  // wakeup; this is what the engine schedules to start the process and what
  // completion is judged against.
  [[nodiscard]] auto TopHandle() const -> CoroutineHandle {
    return coroutine_.Token();
  }

 private:
  ProcessKind kind_;
  Coroutine<void> coroutine_;
  ProcessState state_ = ProcessState::kCreated;
};

}  // namespace lyra::runtime
