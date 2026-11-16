#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// The direction data flows across a port (LRM 23.2.2.3). All four the language
// admits are here because a unit publishes what it declared, not what a
// consumer can currently realize: an `inout` port is a bidirectional net
// connection, and the consumer that cannot yet realize one says so, rather than
// the unit pretending not to have declared it.
//
// A `const ref` is its own direction rather than a flag beside `kRef`: what the
// unit declared is that the connected storage is shared and that writing
// through it is not permitted (LRM 23.3.3.2), and a consumer that admits one
// form and not the other cannot spell that with a value it has to remember to
// check.
enum class PortDirection : std::uint8_t {
  kInput,
  kOutput,
  kInOut,
  kRef,
  kConstRef
};

// The keywords the source declares a port of `direction` by.
[[nodiscard]] auto PortDirectionKeyword(PortDirection direction)
    -> std::string_view;

}  // namespace lyra::support
