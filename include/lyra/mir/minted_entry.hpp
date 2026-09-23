#pragma once

#include <cstdint>

namespace lyra::mir {

// Which body of a unit that answers to no name is meant.
//
// The source declares none of these, and SystemVerilog leaves no spelling
// reserved to the compiler (LRM 5.6.1), so a word minted for one would sit in
// the same name space as that unit's own subroutines. A referrer names one by
// which of them it is instead, so the set and the choice within it are one
// closed set -- and whoever defines one and whoever calls it arrive at the same
// symbol from the unit and that choice, with nothing shared between them.
//
// A unit that is a namespace publishes the first two: `kInstallStorage` gives
// every cell its declared representation and language default and fires
// nothing, then `kInitializeStorage` runs each value initializer through its
// cell (LRM 10.5). A unit whose instances are a tree publishes the third, which
// makes one of those objects for a referrer that cannot -- it consumed what
// that unit promised, and a promise states what may be reached and never how
// much storage an object takes.
enum class MintedEntry : std::uint8_t {
  kInstallStorage,
  kInitializeStorage,
  kMakeObject
};

}  // namespace lyra::mir
