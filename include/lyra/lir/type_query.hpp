#pragma once

#include <optional>

#include "lyra/base/arena.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

using TypeArena = base::Arena<Type, TypeId>;

// The type a reference-like type refers to; absent when the type refers to
// nothing, which is what makes a dereference of it invalid.
auto Pointee(const TypeArena& types, TypeId type) -> std::optional<TypeId>;

// A type whose storage object has no first-class value in LIR: operations on it
// consume its address. A storage cell (an observable variable, a net resolution
// node, a driver slot) and every object-tree node (a class object, a scope, an
// instance) are such a type -- there is nothing to read out of the storage,
// write into it, or hand to a callee except where it lives.
//
// This is about the storage object, not about how a value is represented. A
// packed value is a runtime object reached through an opaque handle, and that
// handle is an ordinary first-class value: a place holding one is loaded and
// stored like any other. The cell that holds a packed value is what is
// address-only, never the value itself.
auto IsAddressOnly(const TypeArena& types, TypeId type) -> bool;

// Whether a callable's result type states the coroutine call protocol: the body
// may hand control back to the scheduler and completes as a coroutine, rather
// than running to a value in one call. The protocol is the type -- nothing else
// records it -- so every layer that must realize suspension or completion asks
// this of the result type.
auto IsCoroutine(const TypeArena& types, TypeId type) -> bool;

}  // namespace lyra::lir
