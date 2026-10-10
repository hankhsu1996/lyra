#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::value {

// A buffer of words a value owns apart from itself. A design makes and ends
// such values many times in a time step, at the lengths its types fix, so a
// buffer that is freed is kept and handed to the next value that asks for one
// as long, which costs less than going to the system's allocator each time.
// What a buffer holds when it is allocated is whatever its last holder left.
[[nodiscard]] auto AllocateWords(std::size_t count) -> std::uint64_t*;
void DeallocateWords(std::uint64_t* words, std::size_t count) noexcept;

}  // namespace lyra::value
