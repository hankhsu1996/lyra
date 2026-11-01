#pragma once

#include <string_view>

#include "lyra/base/fault_watch.hpp"

namespace lyra::base {

// A request for memory the host refuses ends the process where it was made.
// Whatever ran after it might ask again -- a message being composed, an
// exception unwinding through code that does not expect one -- so the process
// runs `before_ending`, which asks for nothing, writes `message`, which needs
// nothing to write, and exits with a failing status, running nothing else.
// Once installed this is what a refused `operator new` does; `message` has to
// outlive the process.
void InstallOutOfMemoryHandler(
    std::string_view message, BeforeEnding before_ending);

// Ends the process as a refused `operator new` does, for a refusal that reaches
// the process another way: a library's own allocator, or a thread whose stack
// the host would not give.
[[noreturn]] void EndForLackOfMemory();

}  // namespace lyra::base
