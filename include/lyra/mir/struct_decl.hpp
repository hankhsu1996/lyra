#pragma once

#include <string>

#include "lyra/base/arena.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// A compiler-generated nominal struct declaration: a name and named fields,
// nothing else. Synthesized by HIR-to-MIR for a promoted automatic scope's
// locals. It is a plain aggregate -- no base, no methods, no dispatch, no
// invoke (a scope is storage, not callable), no lifecycle. A closure is a
// separate category (`ClosureDecl`), not a struct with a body.
//
// How an instance is held -- by value, or through a `Shared` / owned / borrowed
// pointer wrapper -- is the wrapper around the struct's type, never a property
// of this declaration; that a promoted scope is reference storage is expressed
// by the `Shared<>` handle that reaches it, not here.
//
// This declaration fixes only the field storage shape. Constructing an instance
// and assigning its fields is executable scope-entry code, not part of the
// declaration.
struct StructDecl {
  std::string name;
  base::Arena<FieldDecl, FieldId> fields;
};

}  // namespace lyra::mir
