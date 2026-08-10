#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Where a class's C++ text lands. Every class of a unit is emitted at the
// unit's own scope, named by the one identifier it carries, so a reference to
// it reads the same from anywhere the unit can be reached. That requires
// splitting the class in two: a declaration, which every reference and every
// base derivation needs to see, and the member definitions, which need every
// class of the unit complete -- a scope's body reaches its parent's members
// while the parent's own body builds that scope, so neither class can precede
// the other.
struct ClassText {
  std::string declaration;
  std::string definitions;
};

// The two places a callable the unit owns directly lands: its declaration,
// which leads because the classes and bodies below call through it, and its
// definition. The distinction is one the declaration already states -- a
// bodyless callable is an import the user's linked C defines, a bodied one this
// program emits.
struct UnitCallableText {
  std::string declarations;
  std::string definitions;
};

// Every class of the unit, declarations first and definitions after, together
// with the forward declarations that let a field or a signature name a class
// whose own declaration has not been reached yet.
auto RenderUnitClasses(const mir::CompilationUnit& unit) -> ClassText;

// Every callable the unit owns directly -- a package function or task, a DPI-C
// import's prototype, a DPI-C export's entry point -- rendered as free
// functions of the unit's namespace.
auto RenderUnitCallables(const mir::CompilationUnit& unit) -> UnitCallableText;

}  // namespace lyra::backend::cpp
