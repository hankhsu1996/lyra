#pragma once

#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// Pure builders for the types a lowering synthesizes rather than reads off a
// declaration -- stateless, unlike the lowering passes that call them. One test
// decides what belongs here: whether building the type correctly needs a fact
// its fields do not carry. Where every field comes from what the caller already
// knows, the caller interns the alternative directly; where a width, an
// emptiness, a nesting, or a classification has to be right, that fact lives
// here so it is decided once. How many sites want the type is not the test: an
// alternative whose fields are all the caller's own is spelled at each of them
// and shares nothing.

// The storage a local lent by reference lives in, named by the reference that
// reaches it: a reference names the cell a value lives in, so the cell's own
// handle and a reference built over it are one type.
auto ReferenceToCellOf(const TypePool& types, TypeId value_type) -> TypeId;

}  // namespace lyra::lir
