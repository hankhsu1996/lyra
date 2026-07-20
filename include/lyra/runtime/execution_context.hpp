#pragma once

#include <utility>

namespace lyra::runtime {

class RuntimeProcess;

// The process whose body is executing right now. A fork spawn and the LRM 9.6
// process-control statements act on the calling process, and they cannot always
// reach it through a coroutine handle: `fork ... join_none` is legal inside a
// function (LRM 13.4.4), whose body is not a coroutine and can await nothing,
// yet LRM 9.6.1 makes the branches it spawns children of the enclosing process.
// The executing process is ambient runtime state, like the current simulation
// time.
class ExecutionContext {
 public:
  // The base read of the current-process state: the executing process, or null
  // when none is executing. Absence is a legal result the caller handles -- a
  // DPI query reachable from foreign C (`svGetScope`) reads this form because
  // "no imported function is executing" is a state it must report, not fault.
  [[nodiscard]] auto TryCurrentProcess() const -> RuntimeProcess* {
    return current_;
  }

  // The same state read under the invariant that a process is executing (LRM
  // 9.5): a fork spawn or a process-control statement runs inside a process, so
  // absence is a compiler-invariant violation, not a case to handle.
  [[nodiscard]] auto CurrentProcess() const -> RuntimeProcess&;

 private:
  friend class ExecutionContextGuard;

  RuntimeProcess* current_ = nullptr;
};

// Publishes `process` as the executing process for the extent of the scope.
// Restoring the previous process on exit, rather than clearing, makes the host
// call stack the context stack, so a resume that unwinds or that re-enters
// generated code leaves no stale identity behind.
class ExecutionContextGuard {
 public:
  ExecutionContextGuard(ExecutionContext& context, RuntimeProcess& process)
      : context_(&context),
        previous_(std::exchange(context.current_, &process)) {
  }

  ExecutionContextGuard(const ExecutionContextGuard&) = delete;
  auto operator=(const ExecutionContextGuard&)
      -> ExecutionContextGuard& = delete;
  ExecutionContextGuard(ExecutionContextGuard&&) = delete;
  auto operator=(ExecutionContextGuard&&) -> ExecutionContextGuard& = delete;

  ~ExecutionContextGuard() {
    context_->current_ = previous_;
  }

 private:
  ExecutionContext* context_;
  RuntimeProcess* previous_;
};

}  // namespace lyra::runtime
