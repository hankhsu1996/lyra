#pragma once

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
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

// Whether a call names a library entry answering with a reference through
// which the storage it is taken on can be written, so a write through the
// answer changes that storage.
auto AnswersMutableReferenceIntoReceiver(const CallExpr& call) -> bool;

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

// What a place names, as its nodes say (LRM 7.3.2, 13.5.2). Ordinary storage
// has an address of its own. A member of a tagged union, and anything reached
// under one, has none of its own, and neither has a run of bits of a packed
// value, so no reference the program holds ends there. An untagged union's
// members are views of its storage rather than places (LRM 7.3), and its
// storage is ordinary.
enum class PlaceKind : std::uint8_t {
  kOrdinary,
  kNoAddress,
};

// The kind of the storage the place `place` of `block` names. Bits have no
// address; a field step takes the kind of the place it is taken on, or a
// tagged union's member's where it reaches one; a dereference of what a call
// answers into its
// receiver takes the receiver's, and one of a place designated within a write
// takes the kind of what the steps designating it reached; every other place
// is ordinary.
auto KindOfPlace(const CompilationUnit& unit, const Block& block, ExprId place)
    -> PlaceKind;

// A read-only reference to `operand` where it lies: to the storage it names, or
// reads where a borrow may bind that storage, and otherwise to its value, held
// to the end of the full-expression -- as a C++ `const T&` binds an lvalue
// where it lies and a prvalue in a temporary. Bits lie at no address of their
// own, so a borrow of them binds what they hold, as a C++ `const T&` binds a
// bit-field's value in a temporary.
auto Borrow(const CompilationUnit& unit, Builder block, ExprId operand)
    -> ExprId;

// What a place is based on, as rustc names a place by `Place::local` and its
// projections: a local the body declares, a constant of the unit, or a field
// of the closure the body runs on, which the body reaches through its receiver
// as rustc's THIR names a captured variable (`ExprKind::UpvarRef`) the way it
// names a local.
using PlaceBase = std::variant<LocalId, ConstantId, ClosureFieldTarget>;

// What the place `place` of `block` is based on, where nothing but that base
// reaches it: the base itself, then field steps and dereferences of a uniquely
// owning pointer, whose pointee nothing reaches but through its owner. A place
// reached through any other pointer lies wherever the pointer points, and has
// no base unless it is a field of the closure the body runs on. Storage with a
// base is changed only by what names that base.
auto BaseOfPlace(const TypePool& types, const Block& block, ExprId place)
    -> std::optional<PlaceBase>;

// Whether `data` names a constant of the unit, which lies in the read-only
// storage the unit keeps it in, as a C++ `static const` object or a constant
// rustc promotes to static memory does, so a borrow of it refers to it there.
auto NamesAConstant(const ExprData& data) -> bool;

// Whether the value the node `id` of `block` stands for already lies in
// storage -- a place, a read of one that takes the value as it lies, a
// constant of the unit, or what an execution completed with in the storage its
// awaiter handed it -- rather than being made here. Bits lie at no address of
// their own, so a read of them makes the value it answers, as a C++ read of a
// bit-field does.
auto ValueLiesInStorage(const Block& block, ExprId id) -> bool;

// Whether the node `id` of `block` lies in storage or is part of a value that
// does, rather than being or being part of a value made here: a field step is
// part of what it is taken on, and a dereference of a reference a call answers
// into its receiver part of that receiver, as a C++ subobject is part of its
// complete object.
auto WholeLiesInStorage(const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
