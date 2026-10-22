#pragma once

#include <memory>

#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::runtime {

// The execution vehicle that carries an activation's thread across a suspension
// when the scheduler cannot re-enter the activation's coroutine directly: its
// continuation is buried under a native call stack the runtime does not own (a
// DPI foreign task, LRM 35.5.2). Resuming re-enters that native stack on its
// own stack and lets it run on to its next suspension or its return, so the
// scheduler drives this vehicle instead of the coroutine handle.

class ForeignExecution {
 public:
  ForeignExecution() = default;
  ForeignExecution(const ForeignExecution&) = delete;
  auto operator=(const ForeignExecution&) -> ForeignExecution& = delete;
  ForeignExecution(ForeignExecution&&) = delete;
  auto operator=(ForeignExecution&&) -> ForeignExecution& = delete;
  // Defined in this class's own source file, because a class whose virtual
  // functions are all written in a header is emitted into every translation
  // unit that builds one.
  virtual ~ForeignExecution();

  // Re-enter the native stack this vehicle owns and run it on until it yields
  // again or returns. Returns to the caller either way; `IsDone` distinguishes
  // the two. The activation's coroutine is resumed from within that stack, not
  // by the caller.
  virtual void Resume() = 0;

  // Whether the foreign call has returned. False while it is suspended mid-call
  // with more to run; true once its entry has run to completion.
  [[nodiscard]] virtual auto IsDone() const -> bool = 0;

  // What generated code reached from this call allocates and then reads back
  // after a suspension -- the storage an exported subroutine's entry completes
  // into (LRM 35.8). The stretches of the call are separated by parks, so no
  // one stretch's frame lasts long enough for it, and the frames holding it are
  // this stack's, so nothing outside the call is long-lived enough to own it
  // either.
  [[nodiscard]] auto Values() -> ActivationValueStore& {
    return values_;
  }

 private:
  ActivationValueStore values_;
};

// Creates a foreign execution that runs `entry` -- the foreign call -- on its
// own stack. It does not start: the first `Resume` enters it. `entry` returning
// marks it done.
auto MakeForeignExecution(OwnedCall entry) -> std::unique_ptr<ForeignExecution>;

// Suspends the foreign execution currently running on this thread, handing
// control back to whoever last resumed it. Called from within the foreign call
// (the exported-task driver when its SV body suspends across the boundary); the
// matching `Resume` continues just past here. It is an error to call this with
// no foreign execution running on the thread.
void YieldForeignExecution();

// The process whose foreign call is running on this thread right now -- the one
// that entered the current foreign execution. The exported-task driver, reached
// from foreign C with no engine handle, uses it to run the exported body as
// that process's thread (LRM 9.5). It is an error to call this outside a
// foreign call.
auto CurrentForeignProcess() -> RuntimeProcess&;

// Installs `process` as the one whose foreign call is running, for the dynamic
// extent of a scope. The exported-task driver, given no engine handle by
// foreign C, reads it to run its body as that process's thread. The one it
// replaces is restored, so a nested foreign call unwinds to its caller's.
class ForeignProcessGuard {
 public:
  explicit ForeignProcessGuard(RuntimeProcess& process);
  ForeignProcessGuard(const ForeignProcessGuard&) = delete;
  auto operator=(const ForeignProcessGuard&) -> ForeignProcessGuard& = delete;
  ForeignProcessGuard(ForeignProcessGuard&&) = delete;
  auto operator=(ForeignProcessGuard&&) -> ForeignProcessGuard& = delete;
  ~ForeignProcessGuard();

 private:
  RuntimeProcess* previous_process_;
};

// Runs the execution `frame` names to its end on the stack the current foreign
// call is using, `frame` having been entered as its thread's innermost frame.
// That stack cannot be parked the way a coroutine is, so between two stretches
// of the body the vehicle yields to the scheduler (LRM 35.5.1.5), and when the
// scheduler drives it again the thread goes on where it stopped -- its
// innermost frame, which may be a task the body called. A body that consumes
// no simulation time is done on the first resume.
void DriveOnForeignStack(Activation& frame);

// A DPI import task's foreign-call step (LRM 35.5.2): runs `foreign_call` on a
// fiber so an exported task the call reaches can suspend across the boundary,
// enters it on the process the run is currently executing, and answers whether
// the caller must give up control -- which it must only if the call actually
// suspended, since one that returns without consuming time is over before this
// returns. What continues the import frame afterwards is the vehicle rather
// than a wait, because the call is on a stack of its own.
auto RunForeignTaskOnFiber(
    RuntimeEffects& effects,
    OwnedClosure<support::EffectSignature> foreign_call) -> bool;

}  // namespace lyra::runtime
