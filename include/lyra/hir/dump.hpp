#pragma once

#include <string>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/specialization.hpp"

namespace lyra::hir {

// What one specialization of a unit states: its implementation, then how its
// objects are built.
auto DumpHir(const Specialization& lowered) -> std::string;

// What an implementation states, in the same text and without the
// construction of any specialization of it: equal implementations write equal
// text.
auto DumpImplementation(const CompilationUnit& unit) -> std::string;

// What `scope`, a scope of `unit`, states, in the same text: equal scopes
// write equal text, so two scopes whose texts differ are two.
auto DumpScope(const CompilationUnit& unit, const StructuralScope& scope)
    -> std::string;

}  // namespace lyra::hir
