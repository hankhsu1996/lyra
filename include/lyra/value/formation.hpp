#pragma once

#include <cstdint>

namespace lyra::value {

// What reaching an element for a write did to the container holding it. The
// element a write lands in may be one the container already held, one the
// reach itself made -- an associative array entry allocated by being written
// (LRM 7.8.7), a queue element appended at `$+1` (LRM 7.10.1) -- or none at
// all, where an invalid index makes the write ignored (LRM 7.4.6, 7.10.1) and
// what it lands in belongs to no element. The second changes the container
// whatever value is then written, and the third leaves it unchanged whatever
// value is written, so neither can be told from the element's own value.
enum class Formation : std::uint8_t { kExisting, kMade, kNowhere };

}  // namespace lyra::value
