#pragma once

#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// The files one compiled unit becomes: the declarations a referrer compiles
// against, and the translation unit realizing them. Keeping the two apart is
// what lets a unit be compiled with no other unit's bodies present.
//
// The declarations are several files, which is what lets any two units
// reference each other. A file is read once, so it is the unit an order can be
// given to, while what actually has to be ordered is one class against the
// class it rests on. Writing each class the unit promised in a file
// of its own makes the two the same size, and the order among the files is then
// the order among the classes -- which a program always has, since a class may
// not rest on itself.
struct CppUnitArtifacts {
  std::vector<CppArtifact> declarations;
  CppArtifact code;
};

// What one compiled unit becomes. The design root is a unit like any other
// here.
auto EmitCppUnit(const mir::CompilationUnit& unit) -> CppUnitArtifacts;

// The program entry, which constructs the design root and names its
// declarations. A symbol only foreign C calls (LRM 35.7) is defined by the unit
// declaring it, and every such unit is one the program already links, so
// nothing else has to be named here.
auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact;

}  // namespace lyra::backend::cpp
