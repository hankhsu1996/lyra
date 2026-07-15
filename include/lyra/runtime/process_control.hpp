#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/gc_ref.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 9.7 `process::state`, in declaration order: `status()` reports the
// process's state as one of these, and the underlying integer is what the
// SystemVerilog program compares against the enum members.
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
inline auto ProcessSelf(RuntimeServices& services) -> GcRef<RuntimeProcess> {
  return GcRef<RuntimeProcess>(services.CurrentProcess().shared_from_this());
}

// LRM 9.7 `process::status()`: the process's execution state projected onto the
// LRM `state` enum. The state lives on the persistent process node rather than
// the coroutine frame, so a process remains observable through a surviving
// handle after its body terminates.
inline auto ProcessStatus(const GcRef<RuntimeProcess>& self)
    -> lyra::value::PackedArray {
  const ProcessStatusCode code = [&] {
    switch (self->ExecutionState()) {
      case ProcessExecutionState::kCreated:
      case ProcessExecutionState::kRunning:
        return ProcessStatusCode::kRunning;
      case ProcessExecutionState::kWaiting:
        return ProcessStatusCode::kWaiting;
      case ProcessExecutionState::kTerminated:
        return ProcessStatusCode::kFinished;
    }
    throw InternalError("ProcessStatus: unknown process execution state");
  }();
  return lyra::value::PackedArray::Int(static_cast<std::int32_t>(code));
}

}  // namespace lyra::runtime
