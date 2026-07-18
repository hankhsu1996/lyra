#pragma once

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"

namespace lyra::runtime {

class Scope;

// The design context in effect for the duration of a running simulation. A
// foreign C function that calls back an exported SV subroutine (LRM 35.5)
// reaches the wrapper with plain C arguments and no design pointer, so the
// wrapper has nothing to reach the design from unless the runtime anchors it
// ambiently. This holds the design root for that recovery.
//
// It is an RAII stack guard over a thread-local slot: installed once around the
// simulation run at the single run entry, and torn down when the run returns.
// Nested foreign entries (an import called from inside an export) push and pop,
// so the innermost active run is always current. The context is valid only
// while a simulation is running on the current thread; a callback on a
// different thread must install its own.
class AmbientRunContext {
 public:
  explicit AmbientRunContext(Scope* root);
  ~AmbientRunContext();
  AmbientRunContext(const AmbientRunContext&) = delete;
  auto operator=(const AmbientRunContext&) -> AmbientRunContext& = delete;
  AmbientRunContext(AmbientRunContext&&) = delete;
  auto operator=(AmbientRunContext&&) -> AmbientRunContext& = delete;

  [[nodiscard]] auto Root() const -> Scope* {
    return root_;
  }

  static auto Current() -> AmbientRunContext&;

 private:
  Scope* root_;
  AmbientRunContext* previous_;
};

// Resolves the instance an exported subroutine runs against: the child of the
// design root registered under `instance_name`. The exported subroutine is a
// method of that instance, so the wrapper needs the instance as its receiver.
auto ResolveExportInstance(const char* instance_name) -> Scope*;

// Runs an exported SV task's body to completion synchronously and hands back
// its completion payload. A foreign C caller of an exported task (LRM 35.8) is
// not a coroutine, so it cannot `co_await` the body the way an SV enabler does;
// its wrapper enters the body here. The body is resumed once: a task that
// consumes no simulation time runs straight to `final_suspend`, which -- with
// no continuation to transfer to -- lands on `noop_coroutine` and reports
// `done`. A body that suspends across the foreign boundary is the timing /
// disable case (LRM 35.9), which the boundary does not yet carry, so it is
// reported rather than left to hang.
template <class T>
auto RunExportedTaskToCompletion(Coroutine<T> task) -> T {
  task.Handle().resume();
  if (!task.Done()) {
    throw InternalError(
        "a DPI-C exported task that suspends across the foreign boundary is "
        "not yet supported");
  }
  return task.Handle().promise().Take();
}

}  // namespace lyra::runtime
