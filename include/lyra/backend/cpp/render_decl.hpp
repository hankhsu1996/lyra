#pragma once

#include <span>
#include <vector>

#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::backend::cpp {

// A parameter list, each parameter written as its type then its name. Every
// formal is passed by value, a `ref` one included: its type is itself the
// reference, so the aliasing is carried by the type (LRM 13.5.2).
void WriteParameters(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    std::span<const mir::LocalId> params, TargetText& out);

// Text for the unit's headers, which other units include, and text for its code
// file.
struct UnitText {
  TargetText signature;
  TargetText code;
};

// `class C;` for every class of the unit, written ahead of every class so a
// field or parameter can point at a class defined later: into the opening
// header for a class other units may name, into the code file otherwise.
auto RenderUnitForwardDeclarations(const mir::CompilationUnit& unit)
    -> UnitText;

// A class another unit may name, whose text goes into a header of its own.
struct PromisedClass {
  mir::ClassId id;
  TargetText text;
};

// Every class of the unit, split by file. A class another unit may name goes
// into a header of its own; every other class goes into the code file. Member
// function definitions all go into the code file after every class, since one
// may use any class of the unit: a scope's body reads its parent's members,
// and the parent's body builds that scope.
struct UnitClasses {
  std::vector<PromisedClass> promised;
  TargetText internal;
  TargetText definitions;
};

auto RenderUnitClasses(const mir::CompilationUnit& unit) -> UnitClasses;

// The functions of the unit's namespace -- package functions and tasks, DPI-C
// imports, and the entry points of DPI-C exports -- as free functions. An
// import is only declared; the user's C code defines it.
auto RenderUnitCallables(const mir::CompilationUnit& unit) -> UnitText;

// The unit's package variables (LRM 26.2): declared in the header, so other
// units can name them, and defined once in the code file.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit) -> UnitText;

// The global C symbol for each foreign name the unit declares on a scope. Every
// unit declaring that scope writes the same definition, and the linker keeps
// one.
void RenderForeignScopeSymbols(
    const mir::CompilationUnit& unit, TargetText& out);

// `namespace U { class C; }` for each class of another unit whose objects this
// unit points at. A pointer needs only the declaration, so that unit's header
// is not included.
void RenderExternalObjectDeclarations(
    const mir::CompilationUnit& unit, TargetText& out);

}  // namespace lyra::backend::cpp
