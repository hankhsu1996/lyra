#pragma once

#include <cstddef>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Whether a value of `type` is too large to be kept in a frame, which is a
// stack no design chose the size of: such a value is held through a box the
// program allocates, and no body keeps one of its own.
auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool;

// The bytes a value of `type` takes from the most a body's values may take of
// a frame together: its layout where it is a value of the design larger than
// an address, which is what holding it apart would leave in the frame, the
// value a variable's cell keeps inline for the cell, and none otherwise.
// Holding a value no larger than its address apart spares the frame nothing,
// and what the library builds for a call and machine data are as many as the
// calls the source wrote, which a frame keeps as the target's own code keeps
// them.
auto FrameBytesOf(const CompilationUnit& unit, TypeId type) -> std::size_t;

// The nodes of `block` whose values a frame keeps beside its locals, in the
// order the block holds them: each a value one node makes and another reads or
// its statement discards, which is too large for a frame or takes bytes from
// one. Of a value no larger,
// a node computing nothing -- a constant, a declared thing -- names one lying
// elsewhere; and whatever its size, an arm of a choice is made where the
// choice's value is, what a new-expression initializes is made in the storage
// it allocates, and what a suspension waits on is kept by the runtime, so none
// of them is one.
auto ValuesAFrameKeeps(const CompilationUnit& unit, const Block& block)
    -> std::vector<ExprId>;

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
