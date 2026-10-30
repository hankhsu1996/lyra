#pragma once

#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Moves the code of every operation of `cls`, the class `id` belonging to an
// instance (LRM 6.22), into the body class `declaring_frame` stands in, whose
// published class is `published`. Each operation's code becomes that body
// class's answer to the method `published` introduced for it, with the
// instance as its object and what the operation took after it, and the
// operation becomes the entry dispatching on the instance its object belongs
// to. Which body's code runs is then the instance's (LRM 8.25), and `cls`
// states nothing any one body compiled, so every body states it alike.
void MoveCodeIntoBody(
    UnitLowerer& unit_lowerer, mir::ClassId id, mir::Class& cls,
    const WalkFrame& declaring_frame, mir::ClassId published);

}  // namespace lyra::lowering::hir_to_mir
