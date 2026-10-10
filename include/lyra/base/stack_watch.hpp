#pragma once

#include <cstddef>
#include <span>
#include <string_view>

namespace lyra::base {

// How large the alternate stack a thread handles a fault on is.
inline constexpr std::size_t kAltStackSize = std::size_t{64} << 10;

// A thread that overflows its stack faults at an address just past the stack's
// end, and the fault says nothing of why. Once the handler is installed, such a
// fault on a registered thread writes `message` to the error stream, after
// which the process ends as it would have. The handler is installed once for
// the process, and `message` has to outlive the process's threads.
void InstallStackOverflowHandler(std::string_view message);

// Registers the calling thread with the handler: notes where its stack ends,
// and gives it `alt_stack` to handle a fault on, which cannot be the stack
// that overflowed and has to outlive the thread. A thread that already has an
// alternate stack keeps it.
void RegisterThreadStack(std::span<std::byte> alt_stack);

}  // namespace lyra::base
