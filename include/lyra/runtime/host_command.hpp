#pragma once

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// LRM 20.17.1 $system. Executes `command` through the host's command processor
// as if it had been typed at the terminal, and reports what the host answered
// as a PackedArray shaped for SV `int`.
auto RunHostCommand(RuntimeEffects& runtime, const value::String& command)
    -> value::PackedArray;

// LRM 20.17.1 $system called with no argument, which reaches the host with the
// null command: it runs nothing and reports whether a command processor exists
// at all. Running nothing is also why it needs no engine: it observes the host
// and neither reads nor disturbs anything the design has done.
auto RunHostCommand() -> value::PackedArray;

}  // namespace lyra::runtime
