#pragma once

#include <cstdint>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 9.7 `process::state`, in declaration order: `status()` reports the
// process's state as one of these, and the underlying integer is what the
// SystemVerilog program compares against the enum members. The simulated
// program is what tells these apart, never Lyra, so it is not a dispatch set.
enum class ProcessStatusCode : std::int32_t {
  kFinished = 0,
  kRunning = 1,
  kWaiting = 2,
  kSuspended = 3,
  kKilled = 4,
};

// LRM 9.7 `process::self()`: a handle to the process making the call. A task or
// function runs in its caller's thread (LRM 9.5), so this returns the enclosing
// executing process, reached through the ambient execution context.
auto ProcessSelf(RuntimeEffects& runtime) -> value::ObjectRef;

// LRM 9.7 `process::status()`: the process's execution state projected onto the
// LRM `state` enum. The state lives on the persistent process node rather than
// the coroutine frame, so a process remains observable through a surviving
// handle after its body terminates. A terminated process reports FINISHED or
// KILLED by how it terminated -- the completion slot is gone by then, so the
// distinction is read from the node's persistent terminal cause.
auto ProcessStatus(const value::ObjectRef& self) -> lyra::value::PackedArray;

// LRM 9.7 `process::kill()`: forcibly terminate the process and all its
// descendant subprocesses. Each terminated node is marked KILLED and its frame
// released, so nothing can resume it, and every process awaiting one is woken.
//
// Killing the calling process or one of its ancestors is a deferred,
// safe-boundary termination: a running coroutine cannot destroy the frame it is
// executing in, and that frame is somewhere in the killed subtree. Every
// off-path node (each parked at a safe boundary) is torn down synchronously,
// while the chain that owns the running frame is kept linked so it stays alive;
// the running process's own termination is requested (registrations revoked,
// cause recorded) and its body is unwound to the engine's resume boundary,
// where the terminal state is published and the retained chain released.
void ProcessKill(const value::ObjectRef& self, RuntimeEffects& runtime);

// LRM 9.7 `process::await`: wait for another process to terminate, normally or
// forcibly. Termination is monotonic -- the target terminates once -- so
// waiting again after the caller was stopped is the same question asked afresh,
// and a target that has already terminated leaves nothing to wait for. It is an
// error to await the calling process, which cannot wait for its own
// termination.
auto ProcessAwait(const value::ObjectRef& self, RuntimeEffects& runtime)
    -> bool;

// LRM 9.7 `process::suspend()`: pause a process. It is an error to suspend the
// calling process (a function cannot suspend its own execution). Suspending a
// process that is already suspended or terminated has no effect.
void ProcessSuspend(const value::ObjectRef& self, RuntimeEffects& runtime);

// LRM 9.7 `process::resume()`: restart a suspended process. A process that is
// not suspended is unaffected. Otherwise it waits again for the same thing it
// was waiting for, and runs in the current time step where that has already
// happened -- which is the whole of the clause, because a process that was
// runnable when it was stopped is one whose wait was already satisfied.
void ProcessResume(const value::ObjectRef& self, RuntimeEffects& runtime);

}  // namespace lyra::runtime
