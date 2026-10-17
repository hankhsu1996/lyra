#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::runtime {

// A buffer of words a holder keeps a wide value in apart from itself. A design
// builds and ends such holders many times, at the lengths its types fix, so a
// buffer that is freed is kept and handed to the next holder that asks for one
// as long, which costs less than going to the system's allocator each time.
// What a buffer holds when it is allocated is whatever its last holder left.
[[nodiscard]] auto AllocateWords(std::size_t count) -> std::uint64_t*;
void DeallocateWords(std::uint64_t* words, std::size_t count) noexcept;

}  // namespace lyra::runtime
