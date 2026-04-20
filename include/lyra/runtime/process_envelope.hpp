#pragma once

#include <cstdint>

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
// the process object's pre-bound back-pointers. The caller passes the
// dense process id so the envelope can forward it to the body as the
// decision-owner id.
void DispatchAndHandleActivation(
    Engine& engine, RuntimeProcess& proc, uint32_t process_id,
    ResumePoint resume);

}  // namespace lyra::runtime
