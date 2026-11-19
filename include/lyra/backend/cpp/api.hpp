#pragma once

#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/type_operation_bodies.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// The files one unit becomes: the headers other units include, the `.cpp`
// defining everything of the unit's own, and code every unit naming a
// container type writes alike -- the definitions of its class's members --
// which the project keeps one of and compiles once. A unit compiles against
// other units' headers only, never their `.cpp`. The headers are several files
// -- one per class other units may name -- so that two units can include each
// other's headers without a cycle.
struct CppUnitArtifacts {
  std::vector<CppArtifact> declarations;
  CppArtifact code;
  std::vector<CppArtifact> shared_code;
};

// The files one unit becomes; the design root is emitted like any other unit.
// `bodies` are those built for the operations of the run-time-sized containers
// the unit names: each class's members, and the free functions the unit's code
// calls. A construct this target has no form for is reported into `refusals`
// and the rest of the unit is written anyway, so one run reports every such
// construct; files from a unit that reported one are not a program and are not
// written.
auto EmitCppUnit(
    const mir::CompilationUnit& unit, const TypeOperationBodies& bodies,
    diag::DiagnosticSink& refusals) -> CppUnitArtifacts;

// `main.cpp`, which makes the design root and runs it. It needs no other unit:
// a symbol only C code calls (LRM 35.7) is defined by the unit declaring it,
// which the program already links.
auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact;

}  // namespace lyra::backend::cpp
