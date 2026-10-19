#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Holds every value too large for a frame through an owning pointer wherever a
// body would otherwise keep it in its frame -- a local, a parameter, a capture,
// an argument handed to a body, a value made and discarded -- and reaches it
// through that pointer. The source language has no word for where a value
// lives, so the pointer a programmer of a language with one would write for a
// value this large is written here, once, for every consumer to translate.
auto BoxLargeValues(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
