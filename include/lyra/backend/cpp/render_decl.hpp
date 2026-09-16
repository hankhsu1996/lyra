#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Rendered text split by which of a unit's two artifacts it belongs in: the
// signature a referrer compiles against, and the code that realizes it. Every
// renderer producing both answers in this one shape, so where a piece of text
// lands is settled once rather than per kind of declaration.
struct UnitText {
  std::string signature;
  std::string code;
};

// Every class of the unit. The signature carries each class body -- which every
// reference and every base derivation needs to see -- led by the forward
// declarations that let a field or a parameter name a class whose own body has
// not been reached yet; the code carries the member definitions, which need
// every class of the unit complete, since a scope's body reaches its parent's
// members while the parent's own body builds that scope.
auto RenderUnitClasses(const mir::CompilationUnit& unit) -> UnitText;

// Every callable the unit owns directly -- a package function or task, a DPI-C
// import's prototype, the entry point of an export its own namespace defines --
// rendered as free functions of the unit's namespace. A callable with no body
// is an import the user's linked C defines and contributes a prototype alone.
auto RenderUnitCallables(const mir::CompilationUnit& unit) -> UnitText;

// The namespace-level cells the unit owns (LRM 26.2), declared in the signature
// so a referrer names one where it compiles, and defined once in the code so
// the program holds one cell per declaration.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit) -> UnitText;

// The program-global symbol this unit writes for each foreign name it declares
// on a scope. It belongs to no unit, so every unit declaring such a scope emits
// the same text and the party assembling the program keeps one.
auto RenderForeignScopeSymbols(const mir::CompilationUnit& unit) -> std::string;

// The classes of other units whose objects this one's declarations name. Each
// is reached through a pointer, which an incomplete type serves, so the class
// is declared here and the file declaring it goes unnamed.
auto RenderExternalObjectDeclarations(const mir::CompilationUnit& unit)
    -> std::string;

}  // namespace lyra::backend::cpp
