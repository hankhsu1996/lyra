#pragma once

#include <cstdint>

namespace lyra::mir {

// Which of the two bodies a namespace unit publishes for the design root to
// bring its cells up through is meant.
//
// The source declares neither, and SystemVerilog leaves no spelling reserved to
// the compiler (LRM 5.6.1), so a word minted for one would sit in the same name
// space as that unit's own subroutines. A referrer names one by which of them
// it is instead -- and whoever defines one and whoever calls it arrive at the
// same symbol from the unit and that choice, with nothing shared between them.
//
// `kInstallStorage` gives every cell its declared representation and language
// default and fires nothing, then `kInitializeStorage` runs each value
// initializer through its cell (LRM 10.5).
enum class BringUpEntry : std::uint8_t { kInstallStorage, kInitializeStorage };

}  // namespace lyra::mir
