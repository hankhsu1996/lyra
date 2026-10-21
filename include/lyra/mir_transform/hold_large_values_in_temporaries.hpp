#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Makes every value too large for a frame that an expression makes and then
// reads in a temporary: a local of the body, declared by a step of the
// evaluation the value belongs to, in the order that evaluation reads its
// operands, and named where the value is read. A value made where it is read
// would enclose its reader once it is held off the stack, so a chain the
// source writes flat would nest as deep as it is long; made in a temporary, it
// is a run of steps however long it is. A value a borrow refers to that the
// evaluation makes is made in a temporary of it too, since a step ends what
// it makes and the reference may be read after. Which locals are then held
// through a box is the next rewrite's.
auto HoldLargeValuesInTemporaries(mir::CompilationUnit unit)
    -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
