#pragma once

#include <optional>
#include <string_view>

#include "lyra/base/arena.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

using TypeArena = base::Arena<Type, TypeId>;

// The name of a type's kind, for a consumer that must say which type it met and
// could not handle. The kind is the whole answer: an element type or a
// dimension would lengthen the name without changing what the reader does next.
auto TypeKindName(const Type& type) -> std::string_view;

// The type a reference-like type refers to; absent when the type refers to
// nothing. This is the narrow relation of indirection -- storage that lives
// elsewhere -- which is what an address-of yields and a pointer cast retypes.
auto Pointee(const TypeArena& types, TypeId type) -> std::optional<TypeId>;

// The type of the elements a container holds; absent when the type holds no
// elements. Every container names one, whichever way it is indexed and however
// many elements it holds, so a consumer that has to know what a container is
// made of asks here rather than listing the container kinds itself.
auto ElementType(const TypeArena& types, TypeId type) -> std::optional<TypeId>;

// The type of the storage a dereference of `type` reaches. That is a
// reference's referent, and also what a capability wrapper represents -- a
// wrapper is not an indirection to storage elsewhere, it is storage whose
// contents are the value. Absent when the type stands for no storage, which is
// what makes a dereference of it invalid.
auto DerefTarget(const TypeArena& types, TypeId type) -> std::optional<TypeId>;

// A type whose storage object has no first-class value in LIR: operations on it
// consume its address. A storage cell (an observable variable, a net resolution
// node) and every object-tree node (a class object, a scope, an instance) are
// such a type -- there is nothing to read out of the storage, write into it, or
// hand to a callee except where it lives.
//
// This is about the storage object, not about how a value is represented, and
// not about a capability that reaches one. A packed value is a runtime object
// reached through an opaque handle, and a net's driver is a handle naming one
// of a resolution node's slots; either handle is an ordinary first-class value
// its holder owns, so a place holding one is loaded and stored like any other.
// The cell is what is address-only, never the value or the capability reaching
// it.
auto IsAddressOnly(const TypeArena& types, TypeId type) -> bool;

// Whether the type's values are integral: a packed array, or an enumeration
// whose values its base structures. This is the precondition of asking for a
// packed shape, so a consumer that does not already know it asks here first
// rather than listing the integral types itself.
auto IsIntegral(const TypeArena& types, TypeId type) -> bool;

// The packed shape an integral type's value is structured by: a packed array is
// its own shape, an enumeration is represented by its base's. Every consumer
// that must know how an integral value's bits are grouped asks this, so the
// answer is given once rather than re-derived wherever it is needed. A type
// that is not integral has no such shape and is a caller error, never a width
// guess.
auto PackedShape(const TypeArena& types, TypeId type) -> const PackedArrayType&;

// Whether two packed shapes structure their values' bits identically. The
// spelling a shape was declared under is not part of that -- two values of the
// same atom, signedness, and dimensions hold the same bits, so either stands
// where the other does wherever the representation is what is wanted.
auto SameRepresentation(const PackedArrayType& a, const PackedArrayType& b)
    -> bool;

// Whether a callable's result type states the coroutine call protocol: the body
// may hand control back to the scheduler and completes as a coroutine, rather
// than running to a value in one call. The protocol is the type -- nothing else
// records it -- so every layer that must realize suspension or completion asks
// this of the result type.
auto IsCoroutine(const TypeArena& types, TypeId type) -> bool;

}  // namespace lyra::lir
