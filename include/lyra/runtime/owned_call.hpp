#pragma once

#include <functional>

namespace lyra::runtime {

// A call the runtime makes on the program's behalf and is the one holder of --
// the effect a region runs, the entry a fiber starts at. Whatever it carries
// is handed over with it rather than shared, so it is only ever moved.
using OwnedCall = std::move_only_function<void()>;

}  // namespace lyra::runtime
