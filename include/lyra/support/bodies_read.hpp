#pragma once

#include <cstdint>

namespace lyra::support {

// Which instances' bodies a compile reads. The front end elaborates a body for
// an instance only where nothing it already elaborated serves: one agreeing
// with an earlier instance on its definition, on every parameter and on every
// interface it carries, which no name leaves and nothing written elsewhere
// reaches (LRM 23.8, 23.10.1, 23.11, 33.4), is left pointing at that one's
// body. Such an instance depends on nothing but what its instantiation fixes,
// so a compile reads the bodies the front end elaborated and names every other
// instance where it is written. Reading every instance's body instead makes
// the front end elaborate each one, and holds each to its unit; that is how a
// compile is checked against itself.
enum class BodiesRead : std::uint8_t {
  kElaborated,
  kEveryInstance,
};

}  // namespace lyra::support
