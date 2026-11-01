#pragma once

#include <cstddef>
#include <span>
#include <string_view>

namespace lyra::base {

// How large the alternate stack a thread handles a fault on is.
inline constexpr std::size_t kAltStackSize = std::size_t{64} << 10;

// What runs before an ending the process did not choose. A fault runs it
// inside a signal handler, so what it calls there is only what a signal
// handler may call; a refused request for memory runs it in ordinary code.
using BeforeEnding = void (*)() noexcept;

// What runs before an ending where nothing has to be delivered.
void NothingBeforeEnding() noexcept;

// A fault ends the process where it stands: a stack that ran out, a read of
// an address that is not mapped, an instruction the machine refuses, a
// division by zero, an abort. Once the handler is installed, every such fault
// runs `before_ending` and then ends the process as the fault would have.
//
// A thread that overflows its stack faults at an address just past the
// stack's end, and the fault says nothing of why; such a fault on a registered
// thread also writes `overflow_message` to the error stream, after what
// `before_ending` delivered. The handler is installed once for the process,
// and `overflow_message` has to outlive the process's threads.
void InstallFaultHandler(
    std::string_view overflow_message, BeforeEnding before_ending);

// Registers the calling thread with the handler: notes where its stack ends,
// and gives it `alt_stack` to handle a fault on, which cannot be the stack
// that overflowed and has to outlive the thread. A thread that already has an
// alternate stack keeps it.
void RegisterThreadStack(std::span<std::byte> alt_stack);

}  // namespace lyra::base
