#pragma once

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/dpi_scope_registry.hpp"
#include "lyra/runtime/foreign_execution.hpp"

namespace lyra::runtime {

class Scope;
class RuntimeEffects;

// The design context in effect for the duration of a running simulation. A
// foreign C function that calls back an exported SV subroutine (LRM 35.5), or
// that queries the DPI scope surface (LRM 35.5.3, Annex H), reaches the runtime
// with plain C arguments and no design pointer, so it has nothing to reach the
// run from unless the runtime anchors it ambiently. This holds the design root
// for export-instance recovery, the run's effects facade for the executing
// process and time queries, and the DPI scope directory.
//
// It is an RAII stack guard over a thread-local slot: installed once around the
// simulation run at the single run entry, and torn down when the run returns.
// Nested foreign entries (an import called from inside an export) push and pop,
// so the innermost active run is always current. The context is valid only
// while a simulation is running on the current thread; a callback on a
// different thread must install its own.
class AmbientRunContext {
 public:
  AmbientRunContext(Scope* root, RuntimeEffects& effects);
  ~AmbientRunContext();
  AmbientRunContext(const AmbientRunContext&) = delete;
  auto operator=(const AmbientRunContext&) -> AmbientRunContext& = delete;
  AmbientRunContext(AmbientRunContext&&) = delete;
  auto operator=(AmbientRunContext&&) -> AmbientRunContext& = delete;

  [[nodiscard]] auto Root() const -> Scope* {
    return root_;
  }

  [[nodiscard]] auto Effects() const -> RuntimeEffects& {
    return *effects_;
  }

  [[nodiscard]] auto ScopeRegistry() -> DpiScopeRegistry& {
    return scope_registry_;
  }

  static auto Current() -> AmbientRunContext&;

 private:
  Scope* root_;
  RuntimeEffects* effects_;
  DpiScopeRegistry scope_registry_;
  AmbientRunContext* previous_;
};

// Resolves the instance an exported subroutine runs against: the child of the
// design root registered under `instance_name`. The exported subroutine is a
// method of that instance, so the wrapper needs the instance as its receiver.
auto ResolveExportInstance(const char* instance_name) -> Scope*;

// Runs an exported SV task's body to completion and hands back its completion
// payload. A foreign C caller of an exported task (LRM 35.8) is not a
// coroutine, so it cannot `co_await` the body the way an SV enabler does; its
// wrapper drives the body here, on the fiber the foreign call runs on. The body
// runs as the process that entered the foreign call (LRM 9.5). Each time it
// suspends across the boundary -- a delay, an event, a wait -- the fiber yields
// to the scheduler and the body continues when the scheduler drives the fiber
// again; a body that consumes no simulation time completes on the first resume.
template <class T>
auto RunExportedTaskToCompletion(Coroutine<T> task) -> T {
  task.Handle().promise().process = &CurrentForeignProcess();
  task.Handle().resume();
  while (!task.Done()) {
    YieldForeignExecution();
    task.Handle().resume();
  }
  return task.Handle().promise().Take();
}

}  // namespace lyra::runtime
