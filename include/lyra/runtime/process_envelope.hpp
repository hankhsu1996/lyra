#pragma once

#include "lyra/runtime/engine_types.hpp"

namespace lyra::runtime {

class Engine;
struct RuntimeProcess;

// Dispatch a process activation and handle the result.
// Envelope-owned orchestration: resets SuspendRecord, invokes the process
// body, decodes the raw suspend protocol into semantic requests, then calls
// narrow engine scheduling primitives (Delay, InstallTriggers, HandleTrap,
// etc.) to act on the result. All protocol details are confined to this
// function's scope. The frame state and SuspendRecord are reached through
// the process object's pre-bound back-pointers.
void DispatchAndHandleActivation(
    Engine& engine, RuntimeProcess& proc, ResumePoint resume);

}  // namespace lyra::runtime
