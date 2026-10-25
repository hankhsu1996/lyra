#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Makes the temporaries an expression's evaluation needs to run in the order
// its nodes evaluate their operands, each a local of the body declared by a
// step of the evaluation it belongs to and named where it is read.
//
// A value too large for a frame that an expression makes and then reads is
// made in one: made where it is read, it would enclose its reader once it is
// held off the stack, so a chain the source writes flat would nest as deep as
// it is long. A value a borrow refers to that the evaluation makes is made in
// one too, since a step ends what it makes and the reference may be read
// after. And an operand read out of storage that a later operand of the same
// node may change or end -- a value the runtime holds rather than plain bits,
// or the handle the object an operation is entered on is reached through -- is
// taken into one before that operand runs, so the node is handed what was read
// rather than what is there by the time it runs (LRM 8.29 reclaims only an
// object "no longer being used"). Which locals are then held through a box is
// the next rewrite's.
auto OrderEvaluation(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
