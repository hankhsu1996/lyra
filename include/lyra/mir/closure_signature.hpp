#pragma once

#include <optional>

#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::mir {

// The receiver a closure's body is entered on, which is what its kind is, as
// rustc's `closure_env_ty` states one from the other: a read-only borrow of
// the closure for `Fn`, a mutable borrow for `FnMut`, and the box it was made
// in for `FnOnce`, which the body takes over.
[[nodiscard]] auto ClosureReceiverType(
    const TypePool& types, ClosureId closure, support::ClosureKind kind)
    -> TypeId;

// The kind of `closure`, read off the receiver its body is entered on.
[[nodiscard]] auto ClosureKindOf(
    const TypePool& types, const ClosureDecl& closure) -> support::ClosureKind;

// The erased callable `closure` is made into where its type stops being known:
// one of its kind, taking what its body takes and answering what its body
// answers.
[[nodiscard]] auto ErasedTypeOf(
    const TypePool& types, const ClosureDecl& closure) -> TypeId;

// A cast making a closure one of no known type, which is how a closure is
// handed to the library without its type, as rustc's unsizing coercion makes
// a `Box<dyn FnMut>` of a `Box<T>` or a `&dyn FnMut` of a `&T`: the closure,
// and the erased callable it is made.
struct ClosureErasure {
  ClosureId closure;
  TypeId erased;
};

// The erasure a cast from a value of `from` to one of `to` is: `from` holds a
// closure as `to` holds an erased callable -- both a box, or both a borrow of
// one mutability. Nothing for any other cast.
[[nodiscard]] auto ClosureErasureOf(
    const TypePool& types, TypeId from, TypeId to)
    -> std::optional<ClosureErasure>;

}  // namespace lyra::mir
