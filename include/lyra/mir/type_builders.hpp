#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Pure builders for the types a lowering synthesizes rather than reads off a
// declaration -- stateless, unlike the lowering passes that call them. One test
// decides what belongs here: whether building the type correctly needs a fact
// its fields do not carry. Where every field comes from what the caller already
// knows, the caller interns the alternative directly; where a width, an
// emptiness, a nesting, or a classification has to be right, that fact lives
// here so it is decided once. How many sites want the type is not the test: an
// alternative whose fields are all the caller's own is spelled at each of them
// and shares nothing.

// A run of `width` bits whose width follows from an operation -- composing,
// replicating, slicing, counting -- rather than from any declared type. It is
// unsigned, and that is the fact this settles: signedness reaches a value from
// the declaration that named it, and nothing declared these, so treating them
// as signed would invent a sign bit the program never wrote. LRM 11.8.1 says
// the same of the composed case it covers. How many states the bits have does
// not follow from the width, so it is passed in.
auto PackedVectorOf(
    const TypePool& types, std::uint64_t width, IntegralStateKind state_kind)
    -> TypeId;

// The plain-data aggregate of `size` elements: the type of an aggregate
// literal, which is machine data whatever it is later constructed into. The
// count is narrowed to the width the aggregate carries.
auto MachineArrayOf(const TypePool& types, TypeId element, std::size_t size)
    -> TypeId;

// A code address with its prototype erased: no parameters and no result, which
// names an address and says nothing about calling it. This is the type a table
// of entries with differing prototypes holds, and the one every side of such a
// table spells, so erasing and restoring cannot drift apart.
auto ErasedFunction(const TypePool& types) -> TypeId;

// The observable-cell type for a variable of `value_type`: a SystemVerilog
// value-storage data object (LRM 6.5) is an observable cell, so its writes fire
// subscribers and its value is reached through the cell; any other type -- a
// handle, a container, an object, a named event (which carries its own
// subscribe mechanism), a runtime facade, a machine primitive -- is its own
// storage and passes through unwrapped.
auto ObservableCellOf(const TypePool& types, TypeId value_type) -> TypeId;

}  // namespace lyra::mir
