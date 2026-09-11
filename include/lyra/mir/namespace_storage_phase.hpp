#pragma once

#include <cstdint>

namespace lyra::mir {

// Which of the two bodies bringing up a unit's namespace is meant: the one that
// gives every cell its declared representation and default, or the one that
// runs each value initializer through its cell (LRM 10.5). The unit states the
// two as the bodies they are and a call names one of them by this, so the pair
// and the choice between them are the same closed set.
enum class NamespaceStoragePhase : std::uint8_t { kInstall, kInitialize };

}  // namespace lyra::mir
