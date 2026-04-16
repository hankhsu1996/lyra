#pragma once

#include <span>

#include "lyra/runtime/engine_types.hpp"

namespace lyra::runtime {

class Engine;

// Dispatch a process activation and handle the result.
// Envelope-owned orchestration: resets SuspendRecord, invokes the process
// body, decodes the raw suspend protocol into semantic requests, then calls
// narrow engine scheduling primitives (Delay, InstallTriggers, HandleTrap,
// etc.) to act on the result. All protocol details and borrowed spans are
// confined to this function's scope.
void DispatchAndHandleActivation(
    std::span<void*> states, Engine& engine, ProcessHandle handle,
    ResumePoint resume);

}  // namespace lyra::runtime
