#pragma once

#include <optional>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Why this backend cannot realize `unit`, or nothing where it can. A unit
// reaching a property or a behavior through a reference with no class view is
// refused whole rather than emitted with a gap: this backend realizes an object
// as a target-language class and reaches a member by writing its name, so a
// position settled while the design elaborates is one it has no spelling for,
// and what it cannot realize it declines rather than falling back to another
// form.
auto RefusalFor(const mir::CompilationUnit& unit)
    -> std::optional<diag::Diagnostic>;

// The translation unit one compiled unit becomes. The design root is a unit
// like any other here.
auto EmitCppUnit(const mir::CompilationUnit& unit) -> CppArtifact;

// The program entry, which constructs the design root and includes its header.
// A symbol only foreign C calls (LRM 35.7) is defined by the unit declaring it,
// and the root already reaches every such unit -- the namespaces it brings up
// and the design elements it builds -- so nothing else has to be named here.
auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact;

}  // namespace lyra::backend::cpp
