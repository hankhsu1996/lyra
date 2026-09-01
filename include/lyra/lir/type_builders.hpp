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

// The type a reference to a value of `value_type` has. A reference names the
// cell its referent lives in rather than the referent's own value, so the
// pointee is one wrapping further in than the caller states -- the fact this
// answers, and the reason a reference built here and one translated from a
// declaration are the same type. It has to be the cell: what reading and
// writing through the reference mean is the cell's to decide, and an address
// of the value alone could not raise the destination's update event. One
// answer for every reference, since a callee's formal cannot vary with the
// storage its caller lends.
auto ReferenceToCellOf(
    const TypePool& types, TypeId value_type, Mutability mutability) -> TypeId;

}  // namespace lyra::lir
