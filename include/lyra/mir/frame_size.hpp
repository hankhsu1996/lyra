#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Whether a value of `type` is too large to be kept in a frame, which is a
// stack no design chose the size of: such a value is held through a box the
// program allocates, and no body keeps one of its own.
auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool;

// Whether `type` is the address of code taking a value too large for a frame by
// value, which every signature states as a box instead, as every body taking
// one does.
auto TakesAValueTooLargeForAFrame(const CompilationUnit& unit, TypeId type)
    -> bool;

// Whether `call`, answering a value of `result`, enters a body this program
// compiles, which takes each argument too large for a frame in a box: a
// subroutine, a method, a closure, the constructor of an object of this
// program's classes. A runtime entry and a foreign one take what the call
// states as it is, and so does an operation a type answers -- a record's
// method, or the body a unit states for a type with no declaration -- which the
// library reaches through the type's table by a convention of its own.
auto EntersACompiledBody(
    const CompilationUnit& unit, const CallExpr& call, TypeId result) -> bool;

}  // namespace lyra::mir
