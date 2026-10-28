#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Keeps every width a design declares out of its frames: a value too large for
// a frame is held through an owning pointer wherever a body would otherwise
// keep it in its frame -- a local, a parameter, a capture, an argument handed
// to a body, a value made and discarded -- and so is every value a body keeps
// once the ones before it take what a frame may keep of a body's values
// together, a value an expression makes and another reads given a temporary
// first. Each is reached through that pointer. The source language has no word
// for where a value lives, so the pointer a programmer of a language with one
// would write is written here, once, for every consumer to translate.
auto BoundFrames(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
