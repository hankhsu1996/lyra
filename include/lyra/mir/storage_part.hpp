#pragma once

#include <cstdint>
#include <optional>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// How a call names the part it reaches, and nothing for a call that reaches
// none. The entry's own declaration says, so reading and writing one part
// answer alike.
auto SelectionOf(const CallExpr& call) -> std::optional<support::PartSelection>;

// Whether a call names a library entry answering with a reference into the
// storage it is taken on, whose dereference is part of that storage.
auto AnswersReferenceIntoReceiver(const CallExpr& call) -> bool;

// Whether `call`, answering `type`, binds a reference variable to the storage
// its one argument names rather than building a value of it, as a C++
// reference parameter binds an lvalue. One built over nothing binds nothing,
// and is a reference to a value of any type that refers to nothing (LRM
// 15.4.9), as an empty `function_ref` is.
auto BindsReference(const TypePool& types, const CallExpr& call, TypeId type)
    -> bool;

// What a write landing on `target` reaches the part it lands on through, where
// `target` selects a part of a whole, and nothing where `target` names where
// it lands itself. A selection is reached through its whole, so a chain of
// them is followed to the storage the write lands in.
auto SelectedFrom(const Block& block, ExprId target) -> std::optional<ExprId>;

// What a place names, as its nodes say (LRM 7.3, 13.5.2). Ordinary storage
// has an address of its own. A member of a union, and anything reached under
// one, has none of its own, so no reference the program holds ends there. A
// member of an untagged union, and anything reached under one, is also
// overlaid: its bits may be another member's, so it is read only by reading it
// as its type holds a value and written only by a write that leaves it that
// way. Each kind includes what the ones before it say.
enum class PlaceKind : std::uint8_t {
  kOrdinary,
  kNoAddress,
  kOverlaid,
};

// The kind of the storage the place `place` of `block` names. A field step
// takes the kind of the place it is taken on, or a union's member's where it
// reaches one; a dereference of what a call answers into its receiver takes
// the receiver's, and one of a place designated within a write takes the kind
// of what the steps designating it reached; every other place is ordinary.
auto KindOfPlace(const CompilationUnit& unit, const Block& block, ExprId place)
    -> PlaceKind;

// `object` as an operation takes it where it lies -- the object a call is
// entered on, what a borrow binds: the storage it names, or its read where that
// storage is overlaid, since only a read makes a value of its type out of the
// bits there.
auto ReadIfOverlaid(const CompilationUnit& unit, Block& block, ExprId object)
    -> ExprId;

// A read-only reference to `operand` where it lies: to the storage it names, or
// reads where a borrow may bind that storage, and otherwise to its value, held
// to the end of the full-expression -- as a C++ `const T&` binds an lvalue
// where it lies and a prvalue in a temporary.
auto Borrow(const CompilationUnit& unit, Block& block, ExprId operand)
    -> ExprId;

// Whether the value the node `id` of `block` stands for already lies in
// storage -- a place, a read of one that takes the value as it lies, or what
// an execution completed with in the storage its awaiter handed it -- rather
// than being made here. A read of overlaid storage makes the value its type
// holds out of the bits there.
auto ValueLiesInStorage(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool;

// Whether the node `id` of `block` lies in storage or is part of a value that
// does, rather than being or being part of a value made here: a field step is
// part of what it is taken on, and a dereference of a reference a call answers
// into its receiver part of that receiver, as a C++ subobject is part of its
// complete object.
auto WholeLiesInStorage(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
