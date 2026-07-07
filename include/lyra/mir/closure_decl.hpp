#pragma once

#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// A closure declaration: the anonymous concrete callable value a closure site
// constructs. `fields` are the captured bindings; `invoke` is the one body,
// reached over the closure as its receiver (its `locals[0]`, a read-only
// borrow). Callability is the unconditional presence of `invoke` -- a closure
// is always callable, so there is no flag.
//
// Closure and struct share only the field substrate (`FieldDecl` / `FieldId`).
// A closure is not a `StructDecl` with a body: it has no name (it is
// anonymous), it is never a pointee, and a backend realizes it as an anonymous
// callable object (a C++ lambda), not a named nested struct.
struct ClosureDecl {
  base::Arena<FieldDecl, FieldId> fields;
  // A deterministic order over `fields` (a permutation of field ids) that a
  // backend lists captures in. Not a physical layout (offsets belong to LIR)
  // and NOT the initializer evaluation order (that rides `ClosureExpr`): it
  // only pins a stable listing regardless of the order captures were
  // discovered.
  std::vector<FieldId> field_order;
  CallableCode invoke;
};

}  // namespace lyra::mir
