#pragma once

#include "lyra/base/arena.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// A compiler-generated aggregate of storage, and nothing else. Synthesized by
// HIR-to-MIR for a promoted automatic scope's locals. It is a plain aggregate
// -- no base, no methods, no dispatch, no invoke (a scope is storage, not
// callable), no lifecycle. A closure is a separate category (`ClosureDecl`),
// not a struct with a body.
//
// Neither it nor its members carry a name. The source declares no such
// aggregate -- it exists because a lowering gathered locals that outlive their
// body -- so there is no identifier for it to be called by; and a local stays a
// local when its storage moves here, reached by the position it sits at exactly
// as it was before, so gathering it grants it no name either. A backend spells
// both over the identities they sit at.
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
  base::Arena<FieldDecl, FieldId> fields;
};

}  // namespace lyra::mir
