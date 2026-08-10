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

// The runtime header an emitted program's entry translation unit includes,
// spelled the same way. It carries the host boundary, so the entry names only
// the design's own root; naming the header here keeps that one spelling out of
// the emitter's text.
inline constexpr std::string_view kHostEntryHeader =
    "lyra/runtime/simulation_entry.hpp";

}  // namespace lyra::support
