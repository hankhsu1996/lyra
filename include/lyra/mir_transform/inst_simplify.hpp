#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Replaces an operation whose answer is one of its operands with that operand,
// as rustc's and LLVM's InstSimplify do. Reading every part of a value from
// its first, at the value's own kind of part, is reading the value, and
// writing them is writing it, so a select of the whole is the whole wherever
// it stands.
auto InstSimplify(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
