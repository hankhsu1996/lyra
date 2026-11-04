#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"

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

// `width` bits whose width follows from an operation -- composing,
// replicating, slicing, counting -- rather than from any declared type. It is
// unsigned, and that is the fact this settles: signedness reaches a value from
// the declaration that named it, and nothing declared these, so treating them
// as signed would invent a sign bit the program never wrote. LRM 11.8.1 says
// the same of the composed case it covers. How many states the bits have does
// not follow from the width, so it is passed in.
auto PackedVectorOf(
    const TypePool& types, std::uint64_t width, IntegralStateKind state_kind)
    -> TypeId;

// The type a position is stated in (LRM 7.4.5, 11.5.1), which is the type the
// value library reads a position at: signed and four-state at a width no index
// wraps in, so an index of any width shifts by a declared range, and one
// holding x or z stays unknown through every step.
auto PositionType(const TypePool& types) -> TypeId;

// Whether `id` is that type.
auto IsPositionType(const TypePool& types, TypeId id) -> bool;

// The plain-data aggregate of `size` elements: the type of an aggregate
// literal, which is machine data whatever it is later constructed into. The
// count is narrowed to the width the aggregate carries.
auto MachineArrayOf(const TypePool& types, TypeId element, std::size_t size)
    -> TypeId;

// The containers the library declares (LRM 7.5, 7.8, 7.10): a dynamic array of
// `element`, a queue of `element` holding up to `bound`, and an associative
// array of `element` indexed by `index`. Which argument each type is named
// with where is its declaration's, settled here once.
auto DynamicArrayOf(const TypePool& types, TypeId element) -> TypeId;
auto QueueOf(const TypePool& types, TypeId element, support::QueueBound bound)
    -> TypeId;
auto AssociativeArrayOf(const TypePool& types, TypeId index, TypeId element)
    -> TypeId;

// `container`, a container the library declares, holding elements of
// `element`: everything else it is named with -- a queue's bound, an
// associative array's index type -- kept.
auto WithElementType(const TypePool& types, TypeId container, TypeId element)
    -> TypeId;

// The type the library declares by `decl`, which takes no arguments.
auto LibraryTypeOf(const TypePool& types, support::LibraryDecl decl) -> TypeId;

// A code address with its prototype erased: no parameters and no result, which
// names an address and says nothing about calling it. This is the type a table
// of entries with differing prototypes holds, and the one every side of such a
// table spells, so erasing and restoring cannot drift apart.
auto ErasedFunction(const TypePool& types) -> TypeId;

// Whether `id` is that type. Answered against the builder above rather than by
// restating the shape, so a consumer recognizing an erased entry and a producer
// making one cannot describe it differently.
auto IsErasedFunction(const TypePool& types, TypeId id) -> bool;

// An address with its referent erased: it says where something is and nothing
// about what is there. The peer of the entry above and spelled once for the
// same reason -- it is what both sides of a boundary that carries addresses
// without types write, so what one side erases and the other reads back cannot
// drift apart. Reading it as something takes a conversion, which is the step
// that says what was assumed.
auto ErasedPointer(const TypePool& types) -> TypeId;

// A box of `value`: a pointer uniquely owning storage of dynamic duration
// holding one, as Rust's `Box` is.
auto BoxOf(const TypePool& types, TypeId value) -> TypeId;

// What a new-expression yielding `owner` allocates: the pointee of a uniquely
// owning pointer, or the object a class handle refers to. Nothing for a type no
// new-expression yields.
auto AllocatedType(const TypePool& types, TypeId owner)
    -> std::optional<TypeId>;

// The address of the definition every object of one class carries, which the
// unit declaring the class emits as a constant, whichever class it is of,
// through which it is only read.
auto ClassDefinitionPointer(const TypePool& types) -> TypeId;

// The machine function type `code` has, named as code: every parameter it
// takes, its receiver among them, and what it results in.
auto FunctionTypeOf(const TypePool& types, const CallableCode& code) -> TypeId;

// A pointer to the root every object is: how the library hands an object over
// to a body it enters, whatever class the object is of.
auto ObjectRootPointer(const TypePool& types) -> TypeId;

// A pointer to an instance of the design hierarchy, whichever scope it
// realizes, as the library and the code a unit compiles hand one over.
auto ScopePointer(const TypePool& types) -> TypeId;

// The address of the table a constructor of the design hierarchy is handed,
// through which it is only read.
auto ConstructionTablePointer(const TypePool& types) -> TypeId;

// The observable-cell type for a variable of `value_type`: a variable of a
// data type is value storage (LRM 6.5) and so an observable cell, whose writes
// fire subscribers and whose value is reached through the cell; any other type
// is its own storage and passes through unwrapped.
auto ObservableCellOf(const TypePool& types, TypeId value_type) -> TypeId;

}  // namespace lyra::mir
