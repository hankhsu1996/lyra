#pragma once

namespace lyra::runtime {

class Engine;

// Install SIGUSR1 handler that dumps current running process to stderr.
// Must be called after Engine is fully constructed.
void InstallSignalDumpHandler(Engine* engine);

// Remove signal handler and clear engine reference.
void RemoveSignalDumpHandler();

}  // namespace lyra::runtime
