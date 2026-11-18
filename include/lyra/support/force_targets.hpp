#pragma once

#include <cstdint>
#include <utility>

namespace lyra::support {

// What a force and a release take over, as the build reads the standard: a
// whole singular variable, as LRM 10.6.2 admits, or, where the front end is
// told to read a force of a part of a variable tolerantly, any part of one as
// well, each position held as a whole variable's would be.
enum class ForceTargets : std::uint8_t { kWholeVariables, kParts };

// Whether a force may take over a part of a variable.
[[nodiscard]] constexpr auto TakesParts(ForceTargets targets) -> bool {
  switch (targets) {
    case ForceTargets::kWholeVariables:
      return false;
    case ForceTargets::kParts:
      return true;
  }
  std::unreachable();
}

// What a force or a release a reader by name asks for came to (LRM 38.34): the
// part was taken over or let go; the build's reading forbids taking that part
// (LRM 10.6.2); or the reading admits it and no takeover holds it at a fixed
// position, which is not yet carried out. It crosses the library's boundary as
// the machine integer of its enumerator.
enum class TakeoverAnswer : std::uint8_t { kTaken, kForbidden, kNotCarriedOut };

}  // namespace lyra::support
