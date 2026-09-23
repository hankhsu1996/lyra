#pragma once

#include <string>
#include <vector>

#include "lyra/mir/class_id.hpp"
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

// Every declaration of the unit named before it is written out, in the artifact
// whose text names it. It leads each artifact because everything after it may
// name one: a field or a parameter reaching a class whose body has not been
// written yet, and a signature naming the object of the unit it belongs to. A
// forward declaration is a property of the artifact rather than of any one
// declaration in it, which is why it is not produced with the bodies below.
auto RenderUnitForwardDeclarations(const mir::CompilationUnit& unit)
    -> UnitText;

// One class another unit may name, written on its own. Nothing may be written
// beside it: a reader entering files in any order has to be able to reach the
// class this one rests on without reaching this one, and a file it had already
// entered would give it nothing.
struct PromisedClass {
  mir::ClassId id;
  std::string text;
};

// Every class of the unit, split by where each piece is written. A class
// another unit may name is part of what this unit promised, so it is written
// where a referrer compiles, one to a file; every other class is the unit's
// own, however the source named it, and goes with the code. The definitions
// follow every declaration of every kind, since a member definition may name
// any class of the unit -- a scope's body reaches its parent's members while
// the parent's own body builds that scope.
struct UnitClasses {
  std::vector<PromisedClass> promised;
  std::string internal;
  std::string definitions;
};

auto RenderUnitClasses(const mir::CompilationUnit& unit) -> UnitClasses;

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
