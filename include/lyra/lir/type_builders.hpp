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

// The type of the storage a value of `value_type` lives in. Reading and
// writing it are its own operations rather than a load and a store of the
// value, which is what lets a write raise the destination's update event.
auto CellOf(const TypePool& types, TypeId value_type) -> TypeId;

}  // namespace lyra::lir
