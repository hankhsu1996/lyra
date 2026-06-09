#pragma once

namespace lyra::runtime {

class Engine;

// Boundary between a host program and the simulation Engine. Drives a
// bound Engine to completion and converts any escaping C++ exception
// (an InternalError, std::bad_alloc, or other std::exception) into
// EXIT_FAILURE. Never throws.
//
// Engine::Run is the scheduler proper -- its body describes the LRM
// region order and may throw on invariant violation or allocation
// failure. This function is the host-process boundary that catches
// such exceptions and maps them to an exit code. Emitted main.cpp
// calls this rather than Engine::Run directly; any other host program
// that embeds the runtime should do the same.
auto RunSimulation(Engine& engine) -> int;

}  // namespace lyra::runtime
