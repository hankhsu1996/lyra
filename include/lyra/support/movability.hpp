#pragma once

#include <cstdint>

namespace lyra::support {

// Whether a value an evaluation made can be held in a temporary that then
// stands in its place, and how it leaves that temporary. MIR answers it for
// the types it owns, and a library type's declaration states it for that
// type.
enum class Movability : std::uint8_t {
  // A copy holds the whole of it, so it is read out of the temporary.
  kCopyable,
  // It is moved into the temporary and moved out where it is used, since a
  // copy would be a second owner, or nothing holds one.
  kMovable,
  // No temporary stands in for it: it is formed where it is used, and what
  // inside it has to run first is taken ahead instead, as rustc calls a
  // value that may not be moved once made immovable.
  kImmovable,
};

}  // namespace lyra::support
