#pragma once

namespace lyra::runtime {

// Install SIGINT/SIGTERM handlers that set a process-global interrupt flag.
// Handlers only set the flag; they do not print or terminate.
void InstallSimulationInterruptHandlers();

// Restore the previous SIGINT/SIGTERM handlers and clear the flag.
void RemoveSimulationInterruptHandlers();

// Return true if an interrupt was requested since the last checkpoint and
// clear the interrupt flag.
auto ConsumeSimulationInterruptRequested() -> bool;

}  // namespace lyra::runtime
