#pragma once

#include <optional>

#include "lyra/base/arena.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

using TypeArena = base::Arena<Type, TypeId>;

// The type a reference-like type refers to; absent when the type refers to
// nothing. This is the narrow relation of indirection -- storage that lives
// elsewhere -- which is what an address-of yields and a pointer cast retypes.
auto Pointee(const TypeArena& types, TypeId type) -> std::optional<TypeId>;

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

// The packed shape an integral type's value is structured by: a packed array is
// its own shape, an enumeration is represented by its base's. Every consumer
// that must know how an integral value's bits are grouped asks this, so the
// answer is given once rather than re-derived wherever it is needed. A type
// that is not integral has no such shape and is a caller error, never a width
// guess.
auto PackedShape(const TypeArena& types, TypeId type) -> const PackedArrayType&;

// Whether a callable's result type states the coroutine call protocol: the body
// may hand control back to the scheduler and completes as a coroutine, rather
// than running to a value in one call. The protocol is the type -- nothing else
// records it -- so every layer that must realize suspension or completion asks
// this of the result type.
auto IsCoroutine(const TypeArena& types, TypeId type) -> bool;

}  // namespace lyra::lir
