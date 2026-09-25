#pragma once

#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// The files one unit becomes: the headers other units include, and the `.cpp`
// defining everything. A unit compiles against other units' headers only,
// never their `.cpp`. The headers are several files -- one per class other
// units may name -- so that two units can include each other's headers
// without a cycle.
struct CppUnitArtifacts {
  std::vector<CppArtifact> declarations;
  CppArtifact code;
};

// The files one unit becomes; the design root is emitted like any other unit.
// A construct this target has no form for is reported into `refusals` and the
// rest of the unit is written anyway, so one run reports every such construct;
// files from a unit that reported one are not a program and are not written.
auto EmitCppUnit(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> CppUnitArtifacts;

// `main.cpp`, which makes the design root and runs it. It needs no other unit:
// a symbol only C code calls (LRM 35.7) is defined by the unit declaring it,
// which the program already links.
auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact;

}  // namespace lyra::backend::cpp
