#pragma once

#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// A closure declaration: the anonymous concrete callable value a closure site
// constructs. `fields` are the captured bindings. Its one body is the callable
// of the unit's table the closure declares, reached over the closure as its
// receiver (its `locals[0]`, a borrow of the closure or its box, by its kind);
// a closure exists only where that body is synthesized, so it is always
// callable and there is no flag.
//
// A closure shares the field substrate (`FieldDecl` / `FieldId`) with the
// object (`mir::Class`), since a capture is reached by the field id its
// declaration gave it, as a property is. It is not a `RecordDecl` with a body:
// a struct is a value whose components are reached by position and which has no
// invoke, while a closure's one body is part of what it is.
struct ClosureDecl {
  base::Arena<FieldDecl, FieldId> fields;
  // A deterministic order over `fields` (a permutation of field ids) that a
  // backend lists captures in. Not a physical layout (offsets belong to LIR)
  // and NOT the initializer evaluation order (that rides `ClosureExpr`): it
  // only pins a stable listing regardless of the order captures were
  // discovered.
  std::vector<FieldId> field_order;
};

}  // namespace lyra::mir
