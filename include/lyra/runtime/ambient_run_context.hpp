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
// run from unless the runtime anchors it ambiently. This holds the run's
// effects facade for the executing process and time queries, and the DPI scope
// directory built from the design root.
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

  [[nodiscard]] auto Effects() const -> RuntimeEffects& {
    return *effects_;
  }

  [[nodiscard]] auto ScopeRegistry() -> DpiScopeRegistry& {
    return scope_registry_;
  }

  static auto Current() -> AmbientRunContext&;

 private:
  RuntimeEffects* effects_;
  DpiScopeRegistry scope_registry_;
  AmbientRunContext* previous_;
};

// The scope a module-scoped exported subroutine runs against: the current DPI
// scope of the process whose foreign call is executing (LRM 35.5.3). It is the
// instance the foreign call chain established -- the calling context import's
// scope, or one svSetScope redirected to -- and is the exported method's
// receiver. Throws when no scope is current, which means an export was reached
// without a context import or svSetScope (LRM 35.5.3 makes that an error).
auto CurrentExportScope() -> Scope*;

// The run's effects, for a receiver-less package-scoped export (LRM 26.3): with
// no instance to recover, the package free function still takes the engine
// effects as its leading argument, supplied here from the active run context.
auto CurrentExportEffects() -> RuntimeEffects&;

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
