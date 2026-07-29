#pragma once

#include <string_view>

namespace lyra::support {

// The runtime umbrella header, spelled the way an emitted translation unit
// includes it. Emitted code includes exactly this header, and the precompiled
// header pre-parses exactly this header, so the set the emit pulls in and the
// set the PCH covers are the same set. Naming it once is what makes that true:
// were the two sides to spell their own lists, a header reachable from the
// emit but absent from the PCH would be re-parsed for every compiled design
// and nothing would report it.
inline constexpr std::string_view kRuntimePreludeHeader =
    "lyra/runtime/prelude.hpp";

}  // namespace lyra::support
