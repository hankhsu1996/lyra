#pragma once

#include <span>

namespace lyra::base {

// Writes all of `bytes` to the descriptor `fd`, or as much as the descriptor
// takes, retrying a write a signal interrupted. Nothing here is beyond what a
// signal handler may call, which is what makes it the way an ending writes.
void WriteAll(int fd, std::span<const char> bytes) noexcept;

}  // namespace lyra::base
