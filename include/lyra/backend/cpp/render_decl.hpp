#pragma once

#include <span>
#include <vector>

#include "lyra/backend/cpp/scope_view.hpp"
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

// `class C;` for every class of the unit that C++ can declare ahead of its
// definition, written ahead of every class so a field or parameter can point
// at a class defined later: into the forward header for a class other units
// may name, into the code file otherwise. A class declared inside another is
// declared where that one is defined, and nowhere else.
auto RenderUnitForwardDeclarations(const mir::CompilationUnit& unit)
    -> UnitText;

// A class another unit may name, whose text goes into a header.
struct PublishedClass {
  mir::ClassId id;
  TargetText text;
};

// Every class of the unit, split by who reads it. A class another unit may
// name goes into a header, each after the classes it derives from and the one
// it is declared inside, so those sharing a header are in an order that
// compiles; every other class goes into the code file. Member function
// definitions all go into the code file after every class, since one may use
// any class of the unit: a scope's body reads its parent's members, and the
// parent's body builds that scope.
struct UnitClasses {
  std::vector<PublishedClass> published;
  TargetText internal;
  TargetText definitions;
};

// Every render below that writes a body reports into `report` what it has no
// form for, and goes on.
auto RenderUnitClasses(
    const mir::CompilationUnit& unit, UnitRenderReport& report) -> UnitClasses;

// Every closure of the unit as a type of its own: its captures as members and
// its one body. A body that returns is the call operator, run against the
// closure it was called on; one that completes as a coroutine is started
// through a static function taking the closure by value, so the captures live
// in the coroutine's frame for as long as the execution does. Either way the
// body's first local is the closure it reads its captures through. The types
// are declared ahead of every body that builds one, and the bodies after every
// class, since a body may use any of them.
struct UnitClosures {
  TargetText declarations;
  TargetText definitions;
};

auto RenderUnitClosures(
    const mir::CompilationUnit& unit, UnitRenderReport& report) -> UnitClosures;

// The functions of the unit's namespace -- package functions and tasks, DPI-C
// imports, and the entry points of DPI-C exports -- as free functions. An
// import is only declared; the user's C code defines it.
auto RenderUnitCallables(
    const mir::CompilationUnit& unit, UnitRenderReport& report) -> UnitText;

// The structs the unit declares, each the C++ type every unit naming it spells
// it as, with its methods as members. Each goes into a header of its own, which
// a unit naming it includes, and every method is defined once, in the code
// file.
struct UnitStruct {
  mir::StructId id;
  TargetText declaration;
};

struct UnitStructs {
  std::vector<UnitStruct> declared;
  TargetText code;
};

auto RenderUnitStructs(
    const mir::CompilationUnit& unit, UnitRenderReport& report) -> UnitStructs;

// The unit's package variables (LRM 26.2): declared in the header, so other
// units can name them, and defined once in the code file.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit) -> UnitText;

// The global C symbol for each foreign name the unit declares on a scope. Every
// unit declaring that scope writes the same definition, and the linker keeps
// one.
void RenderForeignScopeSymbols(
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    TargetText& out);

}  // namespace lyra::backend::cpp
