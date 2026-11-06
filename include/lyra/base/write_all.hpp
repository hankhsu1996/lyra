#pragma once

#include <span>
#include <system_error>

namespace lyra::base {

// Writes all of `bytes` to the descriptor `fd`, retrying a write a signal
// interrupted, and answers what the descriptor refused, or nothing where it
// took every byte. A write that takes no byte is refused as an I/O error, as
// it would otherwise never end.
auto WriteAll(int fd, std::span<const char> bytes) noexcept -> std::error_code;

// Writes as much of `bytes` to `fd` as the descriptor takes, for a writer with
// nobody to tell what it refused: an ending, from a signal handler or wherever
// else nothing may be called, and a byte that only wakes a reader. Nothing
// here is beyond what a signal handler may call.
void WriteAsMuchAsTaken(int fd, std::span<const char> bytes) noexcept;

}  // namespace lyra::base
