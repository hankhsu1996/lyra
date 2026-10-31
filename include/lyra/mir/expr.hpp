#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/bring_up_entry.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/local_ref.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

struct BuiltinMirTypes;

struct StringLiteral {
  std::string value;
};

// The null borrowed-pointer value. Distinct from a zero integral value: the
// type system carries the pointee identity, and C++ rejects the functional-
// cast construction (`T*()`) that the constructor primitive would otherwise
// produce for a default-init pointer.
struct NullLiteral {};

// A machine-boolean literal: a plain scalar, not a simulation value. It is what
// a runtime entry's signature takes where it wants a plain two-valued flag
// rather than an SV-typed value.
struct MachineBoolLiteral {
  bool value;
};

// A machine-integer literal: a plain scalar, not a simulation value. It is what
// a runtime entry's signature takes where it wants a machine scalar (a bit
// width, an element count) rather than an SV-typed value, so no runtime value
// is ever built for it.
struct MachineIntLiteral {
  std::int64_t value;
};

// A machine-float literal: a plain scalar, not a simulation value. Its own type
// carries the precision it is read back at, so a consumer spelling it never
// asks what it is a constant of.
struct MachineFloatLiteral {
  double value;
};

struct UnaryExpr {
  UnaryOp op;
  ExprId operand;
};

// The operand read as the type this expression has. A cast sits between two
// types and carries both already: the operand's own type is what the value
// comes from, `Expr::type` is what it goes to. That pair is the whole
// statement, so nothing beside it names which cast this is -- a consumer that
// has to tell them apart reads the two types, which is type dispatch and
// belongs where a target answers every other question about a type.
//
// What reaches here moves no simulation value between representations, so
// every target writes it in its own conversion syntax. A destination whose
// representation differs is a reshape, which is a library call that lands at
// that type on its own; reading an SV value out as a machine scalar is a call
// for the same reason; and so is asking whether a handle's object is of a
// class, which only the object answers. So the pairs that arrive are a value
// read as the machine boolean a condition tests (LRM 12.4), a machine integer
// at another, a pointer at another pointee and a code address at the signature
// its definition was generated with, a class handle as one of another class --
// the same object whatever class it is seen as (LRM 8.15, 8.26.5) -- an
// integral value at a type laying it out alike -- an enumeration and its base
// -- and a reference at the same referent read-only; the verification refuses
// every other.
struct CastExpr {
  ExprId operand;
};

struct BinaryExpr {
  BinaryOp op;
  ExprId lhs;
  ExprId rhs;
};

struct ConditionalExpr {
  ExprId condition;
  ExprId then_value;
  ExprId else_value;
};

// A sequence of statements followed by the value the whole yields. It is the
// one node that puts a statement sequence in value position, which is how an
// evaluation of several steps stands where only an expression may: the steps
// run in order, where the expression is written, and the last one names what it
// evaluates to. Every statement form becomes value-producing by standing inside
// one, so this is the only node that lifts statements into value position.
//
// `value` names an expression of `scope`, and is always present: a sequence of
// steps that settles no value is a statement, which is what a block statement
// over the same scope says.
//
// It sequences and nothing else. It is not a callable boundary, so a local
// declared among the steps belongs to the enclosing body and a reference out of
// them needs no capture -- the whole difference from a callable value that
// happens to be invoked at once. And it has no control-flow effect, so the
// steps do not return: a construct whose answer depends on a test states that
// answer as a value the steps settle, never as an exit from the middle. Every
// consumer may therefore run the steps and take the value with no question of
// control leaving from among them.
struct BlockExpr {
  BlockId scope;
  ExprId value;
};

// A write, which yields nothing: its type is `void`, as an assignment's is in
// Rust's MIR. Where the source reads the value of an assignment, the lowering
// binds what the write stores and reads that binding (LRM 11.3.6), so no
// consumer works out what a write yields.
//
// `value` is already typed to match `target`. An assignment operator (LRM
// 11.4.1) is the operator applied to what the target held and then this write,
// which the lowering states as those two steps.
//
// `target` is a place -- a variable, a dereference, or a field step on a place
// -- and the write stores into it. What settles that a write is meant is this
// position, not what the target is. A target is never several places: a source
// language's write to a join of them reaches this layer as a held value and one
// write per place.
struct AssignExpr {
  ExprId target;
  ExprId value;
};

// Identity of a concrete callable at a call site: the class whose callable
// arena declares (or implements) it, and the slot within that arena. Owner is
// the declaring class, not the receiver's class; the two coincide when the
// receiver's class declares the callable itself and diverge when it is
// inherited from a base. A receiver-less callable (a DPI-C import declared in a
// module, a static class method) has no receiver to recover its owner from, so
// the owner is part of the symbol identity the same way; the target shape does
// not split on whether the signature carries `self`. A backend reads the slot's
// name and signature from this stated owner rather than deriving them from the
// receiver's type.
//
// This target is a concrete direct callable identity; a virtual call site does
// not name a `CallableTarget`; virtual dispatch is a separate reference form --
// a logical dispatch slot -- that a receiver and slot together identify.
struct CallableTarget {
  ClassId owner;
  CallableId slot;

  auto operator<=>(const CallableTarget&) const
      -> std::strong_ordering = default;
};

// The body of a closure this unit declares, as clang calls a lambda's
// `operator()`. The call's receiver is what that body is entered on, which its
// receiver parameter states: the closure itself where the body borrows it, and
// the closure's box where the body takes it over -- a body started as a
// coroutine, which outlives the call and ends the box when it ends.
struct ClosureTarget {
  ClosureId closure;

  auto operator==(const ClosureTarget&) const -> bool = default;
};

// Identity of a symbol in the DPI-C name space (LRM 35.4): the program-global
// linkage names imported and exported subroutines resolve to, a name space of
// its own that no compilation-unit scope contains. It is therefore neither a
// callable of this unit nor one of another unit's namespace, and a call names
// it by linkage name alone. A backend renders the unqualified symbol; the
// prototype it resolves against is published once by the unit that declares the
// import.
struct ForeignSymbolTarget {
  std::string linkage_name;

  auto operator==(const ForeignSymbolTarget&) const -> bool = default;
};

// Identity of a receiver-less callable of a unit's namespace -- a package
// function or task (LRM 26.3), and the type-associated functions the compiler
// synthesizes -- whichever unit's namespace defines it: one of another unit's
// is declared among this unit's callables. Its identity is the position its
// declaration sits at, which every body has; being reachable by an identifier
// is a separate relation the namespace holds over that position, stated only
// where the source declared one, so a synthesized body takes no part in it and
// needs no name of the compiler's own. This is the same shape a namespace's
// storage already has, whose pool likewise holds cells the source never named.
struct UnitCallableTarget {
  CallableId slot;

  auto operator<=>(const UnitCallableTarget&) const
      -> std::strong_ordering = default;
};

// Identity of a body another unit's namespace publishes for bringing its cells
// up. The source declares neither of them, so neither can be named (LRM
// 5.6.1); the two ends agree on which is meant by which of them it is, so this
// is its own identity space rather than a name in the unit's. Making one of a
// unit's objects is never called this way: which body is made at an
// instantiation is decided where the program is assembled, so a construction
// reads the entry off the table it was handed.
struct ExternalBringUpTarget {
  std::string unit_name;
  BringUpEntry entry;

  auto operator==(const ExternalBringUpTarget&) const -> bool = default;
};

// The constructor of a class some unit declares, entered on the object the
// call's receiver names: how a constructor constructs its base (LRM 8.7, 8.17),
// as Java's constructor invokes its superclass's on `this`. The object is
// already of its own class, so nothing about it changes as the base's
// constructor runs.
struct ConstructorTarget {
  ClassId cls;

  auto operator==(const ConstructorTarget&) const -> bool = default;
};

// Identity of a method a record's declaration has: the structure or the union,
// and the operation on a whole value the method answers. A record has one
// method per operation, so the pair names the method whichever unit declares
// the record.
struct RecordMethodTarget {
  RecordId record;
  support::ValueOperation answers;

  auto operator==(const RecordMethodTarget&) const -> bool = default;
};

// Identity of the body a unit states for an operation on a whole value of a
// type with no declaration -- a fixed-size unpacked array (LRM 7.4.2) -- or for
// an array element type's default: the type, and the operation. Each unit
// naming the type states that body itself, so the pair names it within the
// unit.
struct TypeOperationTarget {
  TypeId type;
  support::ValueOperation answers;

  auto operator==(const TypeOperationTarget&) const -> bool = default;
};

// Identity of a class field at an access site: the class that declares the
// field, and the slot that class gave it. Owner is the declaring class, not the
// receiver's class; the two coincide when the receiver's class declares the
// field itself and diverge when the field is inherited from a base (LRM 8.14).
// A class another unit defines is declared here with what it published, whose
// properties are the first of its fields in the order it published them, so
// the slot is the same coordinate whichever unit defines the class.
struct ClassFieldTarget {
  ClassId owner;
  FieldId slot;

  auto operator==(const ClassFieldTarget&) const -> bool = default;
};

// Identity of a captured binding, which the closure declaration holds as a
// field like any other storage a declaration declares.
struct ClosureFieldTarget {
  ClosureId owner;
  FieldId slot;

  auto operator==(const ClosureFieldTarget&) const -> bool = default;
};

// A component of a product -- a struct's member, a tuple's component -- or a
// member of a union or a tagged union, by its declaration-order position (LRM
// 7.2, 7.3). Neither has an arena naming its parts: where each lies is the
// type's layout, which the receiver's type states, as rustc projects a field of
// a struct or a union by its index alone.
struct ComponentFieldTarget {
  base::ComponentIndex position;

  auto operator==(const ComponentFieldTarget&) const -> bool = default;
};

// Which field a `FieldAccessExpr` reaches. A field an object or a closure
// keeps is stated as the declaration that declares it and the slot that
// declaration gave it, one alternative per declaration kind, because the
// declaration kinds are what the arenas holding field names are keyed by, and
// a name is resolved by reading the arena the access states rather than by
// classifying what the receiver turned out to be.
using FieldRef =
    std::variant<ClassFieldTarget, ClosureFieldTarget, ComponentFieldTarget>;

// The part a call acts on where the call itself fixes it: a component of a
// product or a member of a tagged union, by position, or a property of an
// object, by the class declaring it. A closure's captures are
// reached only from inside the closure, so no call names one.
using CallPart = std::variant<base::ComponentIndex, ClassFieldTarget>;

// The target of a `Direct` call -- the symbol identity. Each alternative is one
// identity space, told apart by the table that resolves the name: a class this
// unit names, whichever unit defines it (`CallableTarget`), a closure of this
// unit (`ClosureTarget`), a unit's namespace, whichever unit it is
// (`UnitCallableTarget`), the closed set of runtime library entries
// (`BuiltinFn`), a class's constructor (`ConstructorTarget`), a declared
// struct's or union's methods (`RecordMethodTarget`), the operations this unit
// states for a type with no declaration (`TypeOperationTarget`), the bodies
// another unit's namespace is brought up through (`ExternalBringUpTarget`), and
// the DPI-C name space (`ForeignSymbolTarget`, LRM 35.4). Nothing here says
// whether the call dispatches on an object -- that is the callee's receiver --
// and none is recovered from the receiver's runtime type.
using DirectTarget = std::variant<
    CallableTarget, ClosureTarget, UnitCallableTarget, support::BuiltinFn,
    ConstructorTarget, RecordMethodTarget, TypeOperationTarget,
    ExternalBringUpTarget, ForeignSymbolTarget>;

// A direct call whose callee is settled at compile time. The single shape for
// every direct invocation: a user method, a built-in, a subroutine of this
// unit's namespace or of another's, a name in the DPI-C space.
//
// The object the call dispatches on rides here, distinct from user-supplied
// `CallExpr::arguments`, so the call carries exactly the arguments the SV
// source wrote and the receiver is not conflated with them. It is absent for a
// call that dispatches on nothing -- a type-associated method (LRM 8.10), a
// package subroutine (LRM 26.3), a runtime entry the program reaches by name.
// The target says where the code is found and this says what it is applied to,
// so the two vary independently. It is the object itself, as a field access's
// receiver is: one a pointer reaches is dereferenced to it first, so `p->f()`
// is a call on `*p`.
// `part` names the part an entry acts on where that part is fixed by the call
// itself. It rides with the callee rather than among the arguments because the
// part named has a type of its own, so a target with a type system settles the
// part where it settles types -- which is naming the operation, not handing it
// a value. Absent for every entry that names no part.
// `type_arguments` are the types a library entry generic over a type is called
// at, as a C++ call names a template's arguments: the destination of a
// conversion, the type some bits of a value are read or written at. They are
// part of which function is called, and what the call answers with follows
// from them. Empty for every other callee.
struct Direct {
  DirectTarget target;
  std::optional<ExprId> receiver = std::nullopt;
  std::optional<CallPart> part = std::nullopt;
  std::vector<TypeId> type_arguments = {};
};

// A call through a code address the program computed -- the indirect-call
// shape, the dual of `Direct`. `code` evaluates to the address, which is
// machine data like any other, so nothing about which body runs is known where
// the call is written. The entry an export publishes reaches its
// implementation this way (LRM 35.5.4), the address having been resolved by
// name.
struct Indirect {
  ExprId code;
};

// A virtually-dispatched call: the receiver -- the object, as a direct call's
// is -- is evaluated once and then the body the object's class gives the
// behavior `slot` runs (LRM 8.20). A behavior an interface class introduces is
// one too, which the object's class answers for that interface class (LRM
// 8.26.5); where that body is found is the realization's, as clang's one
// member call becomes a table load only in code generation. The receiver rides
// here, distinct from user-supplied `CallExpr::arguments`, so the call carries
// exactly the arguments the SV source wrote and the receiver is not conflated
// with them.
struct Virtual {
  ExprId receiver;
  VirtualSlot slot;
};

// Builds a value of the call's result type from the positional arguments --
// the library type's own way of coming into existence, which each type has
// exactly one of, so naming the type names the entry (Python's `T(args)`,
// Rust's `T::new(args)`). A value that is instead its own parts is a
// value-build primitive and never reaches here, and a value whose type has
// more than one way to be built names which one through `Direct`.
struct Construct {};

// One call site's invocation semantics. The arm is a property of the call
// node, independent of the callee's own dispatch-family membership: a
// super-qualified call to a virtual-family method carries `Direct` because
// the source demands the base implementation regardless of the receiver's
// dynamic type, while a plain unqualified call to the same method carries
// `Virtual`. A backend reads the invocation semantic from the arm and never
// re-derives it from the callee declaration.
using Callee = std::variant<Direct, Indirect, Construct, Virtual>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

// Where a placement new-expression's storage comes from: the allocation
// function, a library entry, and the arguments handed to it.
struct PlacementAllocation {
  support::BuiltinFn allocate;
  std::vector<ExprId> arguments;
};

// Allocates storage for one value of the pointee type and initializes it from
// `initializer`, whose value is formed in that storage rather than handed over
// as an argument: C++'s `new T(init)`, and SV's `new` (LRM 8.7). `Expr::type`
// is what owns the storage from there -- a uniquely owning pointer, or a class
// handle -- and `initializer` has the pointee's type. An object's initializer
// is its construction from its constructor's arguments. The object is first
// created whole, every member of every class of its lineage in existence and
// its own class's tables in place, and its constructor is then entered on it,
// so it is of its own class while every constructor runs (LRM 8.17).
//
// `placement`, where present, is where the storage comes from instead of the
// heap: a placement new-expression, `new (args) T(init)`, which names the
// allocation function it calls and the arguments handed to it, as clang's
// `CXXNewExpr` names its `operator new` and its placement arguments. The
// function is asked for storage of the size and alignment of a value of the
// pointee type, which the backend supplies, and owns that storage, so the
// expression answers a pointer owning nothing, and an initializer that raises
// gives nothing back (no deallocation function matches, as in C++).
struct NewExpr {
  ExprId initializer;
  std::optional<PlacementAllocation> placement = std::nullopt;
};

// Names the place its operand stands for: the object a borrowed pointer or a
// managed handle refers to, or the storage a capability wrapper represents.
// This is place formation rather than an operation -- reading it loads the
// referent, storing into it writes the referent, a descent rooted at it reaches
// part of the referent, and passing it by reference lends the referent.
// The bare operand keeps naming the pointer or the wrapper itself, so rebinding
// one is a store carrying no dereference. `Expr::type` is the referent's type.
// Taking the address of a dereferenced managed handle is how a borrowed pointer
// to a managed object is obtained.
struct DerefExpr {
  ExprId pointer;
};

// Takes the address of a place expression, yielding a borrowed pointer to
// that storage. The dual of `DerefExpr`. `operand` must be an addressable
// place -- a reference to storage, a dereference, or a field step on ordinary
// storage; a value names no storage, and neither does a union's member (LRM
// 13.5.2).
// `Expr::type` is `PointerType{ ownership = kBorrowed, pointee = operand.type
// }`. `AddressOf(Deref(p))` collapses to `p` at backend lowering when `p` is a
// borrowed pointer (the round-trip is a no-op); over a managed handle it does
// not collapse, since the address of the handle's object is not the handle.
struct AddressOfExpr {
  ExprId operand;
};

// Binds a reference to its operand, as Rust's `&operand` does: `Expr::type` is
// a `ReferenceType` to the operand's type. A place is referred to where it
// lies. A value names no place, so it is held for the reference in storage
// that lasts to the end of the full-expression, as a C++ reference bound to a
// temporary holds it (clang's `MaterializeTemporaryExpr`). Reading through the
// reference is a `DerefExpr` over it.
struct BorrowExpr {
  ExprId operand;
};

// A consuming (transfer) read of a place, rustc's `Operand::Move`: its value
// flows into the enclosing expression together with the place's ownership of
// it, so the place owns nothing afterwards, keeps no object in use, and ends
// with nothing to release. `Expr::type` is the operand's type -- a move changes
// how ownership crosses, not the value. The unit's verification takes it only
// of a whole local, as that local's last use on every way that reaches the
// local's end, run exactly once there. An ordinary read is a copy.
//
// Operand must be a type whose value can be transferred. Alias-style handle
// types carry no ownership to move, so lowering never wraps them here -- a
// move primitive over an alias is a semantic type error.
struct MoveExpr {
  ExprId operand;
};

// The value a place holds, read where it lies: clang's lvalue-to-rvalue
// conversion, rustc's `Operand::Copy`. A place stands in a value position only
// under this node or a move, so where storage is read is stated rather than
// worked out from where a place happens to stand. `Expr::type` is the place's
// type.
struct CopyExpr {
  ExprId place;
};

// The position of the member the tagged union `tagged` holds: rustc's
// `Rvalue::Discriminant`, which its MIR reads before it switches on a variant
// (LRM 11.9). The union is taken as an object an operation is entered on, read
// where it lies when it names storage. Whether it holds a given member is this
// compared with that member's position. `Expr::type` is a machine integer.
struct DiscriminantExpr {
  ExprId tagged;
};

// Field access on an explicit receiver: `receiver.field`. The receiver is the
// object of whichever declaration the field names, or the product or union
// whose component or member it is, as a place; a value a computation makes is
// held for the full-expression, as C++ materializes the base of a member
// access. An object a handle, a pointer, or a write in progress reaches is
// first dereferenced to it, so `p->field` is this node over `*p`, as in rustc's
// MIR, where a field is projected from a place that is itself a dereference. So
// a backend never asks "what is the current receiver?", never asks whether the
// receiver must first be opened, and never derives which arena declares the
// name from what the receiver's type turns out to be.
//
// One node serves reading the field and writing it. Which of the two an
// occurrence is follows from what takes it -- a read reads it, an assignment
// writes it -- exactly as it does in the languages MIR is a peer of, where
// `u.f` is one syntax and its value category settles the rest.
struct FieldAccessExpr {
  ExprId receiver;
  FieldRef field;
};

// A value that is its parts and nothing more, composed from them in order. It
// decomposes into nothing further, which is what makes it a primitive rather
// than a call: no entry is named and no library is asked to do anything.
//
// Which value is composed is `Expr::type`, the way a brace initializer's
// meaning is the type it initializes -- a product from its components, a
// contiguous element list from its elements. Each of those has exactly one way
// to be built, so the type answers completely and nothing chooses. A value that
// has more than one way to come into existence is not this node: a union is
// built by the expression naming its member, and a library type through its own
// constructor with this list among the arguments.
struct CompositeExpr {
  std::vector<ExprId> parts;
};

// A union's value holding the member at `member` as `value`, which an
// assignment pattern or a tagged union expression names (LRM 7.3, 11.9), as
// clang's initializer list names the field of a union it initializes and
// rustc's aggregate the field a union expression gives. Every other byte of the
// union's storage is zero, since a read through another member may read it
// (LRM 7.3.2), and a tagged union's tag names the member. `Expr::type` is the
// union.
struct UnionExpr {
  base::ComponentIndex member;
  ExprId value;
};

// Waiting for an execution to reach its end (LRM 13.3 task enable): the
// execution `execution` evaluates to takes over the thread, and control comes
// back here once it completes, with the value it completed with. `Expr::type`
// is that value -- a task's output pack, or `Void` for one with no outputs.
//
// Invariant, shared with `WaitExpr`: it appears only at statement top level --
// as the expression of an `ExprStmt`, or as the right-hand side of the
// local-decl / assignment that binds its completion value -- because SV
// suspends only at statement position (LRM 13.4). It is an expression rather
// than a statement because it yields a value.
struct AwaitExpr {
  ExprId execution;
};

// Stopping this execution where `park` says to (LRM 9.4 timing controls, 9.6
// `wait fork`, 9.7 `await`, a foreign task's fiber): `park` is a call that has
// parked the execution and arranged how it resumes, and answers whether
// control must be given up at all, so control leaves here where it answers
// true and comes back when the execution is resumed. Yields nothing. A
// different operation from awaiting an execution: what ends the wait is the
// scheduler rather than another body, and nothing is handed back.
struct WaitExpr {
  ExprId park;
};

// Projects one element out of a sequence value by position. The position is an
// operand rather than part of the node because a sequence is homogeneous:
// which element is named cannot change the element's type, so nothing about
// the projection has to be known at compile time. It extracts the element from
// the sequence value; a sequence of storage is reached through the indirection
// its elements already carry, not by addressing into the sequence itself.
struct VectorGetExpr {
  ExprId vector;
  ExprId index;
};

// The definition every object of one class carries, which the unit defining
// the class emits as a constant. Only a class some unit declares has one, and
// whichever unit defines it, it is reached the same way. `Expr::type` is the
// runtime class definition itself, so a use wanting its address takes it.
struct DefinitionRef {
  ClassId of;
};

// The members an enumeration declares, named by the entry holding them: what
// the questions LRM 6.19.5 and 6.24.2 ask of a value are answered against. The
// members are settled at compile time and shared by every question about that
// enumeration, so the unit states them once and a use names which table.
// `Expr::type` is the library's member table.
struct EnumTableRef {
  EnumTableId table;
};

// A constant integral value, named by the entry holding it. The value is
// settled before the program runs and shared by every use that reaches it, so
// a use says which one. `Expr::type` is the constant's own integral type.
struct IntegralConstantRef {
  IntegralConstantId constant;
};

// A place naming a class's static property (`Class::name`, LRM 8.9): a mutable
// type-associated storage cell. `owner` is the class whose static-property
// arena declares the cell (a derived source access like
// `Derived::inherited_prop` still names the base class here, mirroring the
// owner-qualification rule for inherited instance access). `Expr::type` is the
// property's observable-cell type, so a read wraps it in `Get` and a write in
// `Set`: the property needs no object, so a name outside every body of the
// class reaches it and a continuous assignment may take it as an operand
// (LRM 10.3.2) -- which is what a plain value cannot answer for and an instance
// property, reachable only through a receiver, never has to.
struct StaticPropertyRef {
  ClassId owner;
  StaticPropertyId prop;
};

// A place naming a variable a unit's namespace owns (LRM 26.2), by the position
// the variable's declaration sits at, whichever unit's namespace owns it: one
// of another unit's is declared among this unit's cells. A namespace has no
// instance and no receiver, so the variable is reached directly rather than
// through a `self`-based field access. `Expr::type` is the variable's
// observable-cell type, so a read wraps it in `Get` and a write in `Set`.
struct StaticVariableRef {
  StaticVariableId variable;
};

// A constant a class of this unit holds, by the position it sits at in that
// class's arena. `Expr::type` is the constant's own type.
struct ClassConstantRef {
  ClassId owner;
  ClassConstantId constant;

  auto operator==(const ClassConstantRef&) const -> bool = default;
};

// A body of a class this unit defines, named as the code it is rather than
// called. `Expr::type` is the machine function type the body has: every
// parameter it takes, the receiver among them, and what it results in. Read as
// a value it is the body's address, the way a function's name is in C.
struct FunctionRef {
  CallableTarget body;
};

// The entry that makes an object of one body of another unit, or a block of
// one block body of it, named as the code it is, as `FunctionRef` names a body
// of this unit. The source declares no such entry, so it is named by the body
// it makes and the unit that body realizes, and every target spells it from
// those. `Expr::type` is the machine function type the unit published the
// entry with, not the prototype-erased type a construction table holds it at,
// which is a conversion of this.
struct ExternalObjectEntryRef {
  std::string unit_name;
  std::string body_name;
  std::optional<BlockBodyOrdinal> block;
};

// The entry of the method `body`: the body, stated by the class defining the
// method, that is handed the object as the root every object is and calls the
// method on it, which is how the runtime library and a dispatching call reach a
// method of a class they do not know. `body` is a method of a class some unit
// declares, whichever unit defines it, so an entry is named by the method it
// enters. `Expr::type` is the entry's machine function type.
struct DispatchEntryRef {
  CallableTarget body;
};

// What ends an object of the class `of`: each member, the class's own and then
// those of what it extends, and then the storage the object was created in, as
// a deleting destructor does. `of` is a class some unit declares that is no
// interface class, whichever unit defines it. `Expr::type` is its machine
// function type, taking the object as the root every object is.
struct DestructorRef {
  ClassId of;
};

// The table of the bodies an object of the class `of` runs for the behaviors
// it dispatches on (LRM 8.20), and the table of those answering what the
// interface class `iface` declares itself on an object of `of` (LRM 8.26.5),
// each as the address the class's definition holds. Which entry fills each
// place is what `of` states of its dispatch; how a table is laid out is the
// target's to decide, so neither is a value this layer builds. `of` is a class
// this unit defines. `Expr::type` is a pointer to read-only data of no type.
struct DispatchTableRef {
  ClassId of;
};

struct InterfaceTableRef {
  ClassId of;
  ClassId iface;
};

// Which declared thing a reference names. The alternatives differ in the table
// that resolves the name -- a body's own bindings, a class's arena, another
// unit's signature, the constants this unit holds -- and that is the
// referent's business: every one of them reaches storage, a constant or code
// that exists whether or not this expression names it.
using ReferenceTarget = std::variant<
    LocalRef, DefinitionRef, EnumTableRef, IntegralConstantRef,
    StaticPropertyRef, StaticVariableRef, ClassConstantRef, FunctionRef,
    DispatchEntryRef, DestructorRef, DispatchTableRef, InterfaceTableRef,
    ExternalObjectEntryRef>;

// Names a declared thing. Reading it loads what the name reaches, assigning to
// it stores there, and taking its address yields a pointer to it -- one node
// for all of that, because which of them an occurrence is follows from where it
// stands, exactly as it does for a field access.
struct ReferenceExpr {
  ReferenceTarget target;
};

using ExprData = std::variant<
    StringLiteral, NullLiteral, MachineBoolLiteral, MachineIntLiteral,
    MachineFloatLiteral, ReferenceExpr, UnaryExpr, BinaryExpr, CastExpr,
    ConditionalExpr, BlockExpr, AssignExpr, CallExpr, NewExpr, DerefExpr,
    AddressOfExpr, BorrowExpr, MoveExpr, CopyExpr, DiscriminantExpr,
    FieldAccessExpr, ClosureExpr, CompositeExpr, UnionExpr, AwaitExpr, WaitExpr,
    VectorGetExpr>;

// `span` is the piece of source the node was lowered from, or no place where
// it was lowered from none, as the front end gave it; only the front end opens
// it.
struct Expr {
  ExprData data;
  TypeId type;
  diag::SourceSpan span{};
};

[[nodiscard]] inline auto MakeLocalRefExpr(LocalId var, TypeId type) -> Expr {
  return Expr{
      .data = ReferenceExpr{.target = LocalRef{.var = var}}, .type = type};
}

// Whether `data` names storage rather than making a value, which is what its
// kind is: a reference to storage, a dereference, or a field of a place, as
// rustc's `Category::of` reads a THIR expression's kind. A reference to a
// constant or to code names something that is no storage, so it is a value.
[[nodiscard]] auto IsPlace(const ExprData& data) -> bool;

// Whether `data` is a constant: a value the same wherever and whenever it is
// evaluated, which is what its kind is -- a literal, or a reference to a value
// the unit states once, such as an integral constant, an enumeration's table
// or a body's code. Nothing about such a node changes between two evaluations,
// so it computes nothing and may be stated again wherever it is wanted, as C++
// names a variable usable in constant expressions without odr-using it.
[[nodiscard]] auto IsConstant(const ExprData& data) -> bool;

// The value the place `place` of type `type` holds.
[[nodiscard]] inline auto MakeCopyExpr(ExprId place, TypeId type) -> Expr {
  return Expr{.data = CopyExpr{.place = place}, .type = type};
}

// The local a reference names, for a consumer following where storage lives;
// absent for a reference to anything a body does not bind.
[[nodiscard]] inline auto ReferencedLocal(const ExprData& data)
    -> std::optional<LocalId> {
  const auto* reference = std::get_if<ReferenceExpr>(&data);
  if (reference == nullptr) {
    return std::nullopt;
  }
  const auto* local = std::get_if<LocalRef>(&reference->target);
  return local != nullptr ? std::optional{local->var} : std::nullopt;
}

// The library entry a call names outright, if it names one. A call whose callee
// is anything but a library entry names none, so a consumer asking which
// operation this is gets nothing rather than a guess.
[[nodiscard]] inline auto DirectBuiltinFn(const CallExpr& call)
    -> std::optional<support::BuiltinFn> {
  const auto* direct = std::get_if<Direct>(&call.callee);
  if (direct == nullptr) {
    return std::nullopt;
  }
  const auto* fn = std::get_if<support::BuiltinFn>(&direct->target);
  return fn != nullptr ? std::optional{*fn} : std::nullopt;
}

// The callee of a call to the library entry `entry` that answers with a value
// of `answer`. An entry generic over the type it answers with is called at
// that type; any other states no type.
[[nodiscard]] inline auto BuiltinCalleeAnswering(
    support::BuiltinFn entry, std::optional<ExprId> receiver, TypeId answer)
    -> Direct {
  return Direct{
      .target = entry,
      .receiver = receiver,
      .type_arguments = support::RuntimeEntryOf(entry).takes_a_type_argument
                            ? std::vector{answer}
                            : std::vector<TypeId>{}};
}

[[nodiscard]] inline auto MakeFieldAccessExpr(
    ExprId receiver, FieldRef field, TypeId type) -> Expr {
  return Expr{
      .data = FieldAccessExpr{.receiver = receiver, .field = field},
      .type = type};
}

// A write of `value` into `target`. Typed `void`, which is the whole of what a
// write yields.
[[nodiscard]] auto MakeAssignExpr(
    const BuiltinMirTypes& builtins, ExprId target, ExprId value) -> Expr;

// The object the call dispatches on, absent for a call that dispatches on
// nothing. The one place the question is answered, so no consumer works out
// which operand a receiver is from a signature, a namespace, or an operand's
// type.
[[nodiscard]] auto CalleeReceiver(const Callee& callee)
    -> std::optional<ExprId>;

// `callee(arguments...)`, answering `type`.
[[nodiscard]] inline auto MakeCallExpr(
    Direct callee, std::vector<ExprId> arguments, TypeId type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = std::move(callee), .arguments = std::move(arguments)},
      .type = type};
}

// `lyra::runtime::current_runtime()` -- reaches the attached Runtime's
// capability view through a thread-local pointer the Runtime publishes for
// its lifetime. Zero-argument free function so every body kind -- module
// process, class method, package function, class static method -- takes one
// uniform runtime-access path with no receiver.
[[nodiscard]] inline auto MakeCurrentRuntimeCallExpr(TypeId effects) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kCurrentRuntime},
              .arguments = {}},
      .type = effects};
}

// `new T(initializer)`, owned by `owner`.
[[nodiscard]] inline auto MakeNewExpr(ExprId initializer, TypeId owner)
    -> Expr {
  return Expr{.data = NewExpr{.initializer = initializer}, .type = owner};
}

// `new (arguments) T(initializer)` in storage `allocate` hands out, answering
// `place`, a pointer owning nothing.
[[nodiscard]] inline auto MakePlacementNewExpr(
    ExprId initializer, PlacementAllocation placement, TypeId place) -> Expr {
  return Expr{
      .data =
          NewExpr{
              .initializer = initializer, .placement = std::move(placement)},
      .type = place};
}

// The construction of an object of `object_type` by its constructor, which a
// new-expression initializes the storage it allocates with.
[[nodiscard]] inline auto MakeObjectConstructionExpr(
    TypeId object_type, std::vector<ExprId> constructor_arguments) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Construct{},
              .arguments = std::move(constructor_arguments)},
      .type = object_type};
}

// `*pointer` -- names the storage `pointer` reaches. `referent_type` is what
// that storage holds: the pointee for a pointer or handle.
[[nodiscard]] inline auto MakeDerefExpr(ExprId pointer, TypeId referent_type)
    -> Expr {
  return Expr{.data = DerefExpr{.pointer = pointer}, .type = referent_type};
}

// A check an access owes, stated before the access: raises `message` as a
// run-time error unless the machine boolean `condition` holds, as rustc's
// `Assert` stands before the projection it guards.
[[nodiscard]] inline auto MakeAssertExpr(
    ExprId condition, ExprId message, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kAssert},
              .arguments = {condition, message}},
      .type = void_type};
}

// The position of the member the tagged union `tagged` holds.
[[nodiscard]] inline auto MakeDiscriminantExpr(
    ExprId tagged, TypeId machine_int) -> Expr {
  return Expr{.data = DiscriminantExpr{.tagged = tagged}, .type = machine_int};
}

// A union's value holding `value` as its member `member`.
[[nodiscard]] inline auto MakeUnionExpr(
    ExprId value, base::ComponentIndex member, TypeId built) -> Expr {
  return Expr{
      .data = UnionExpr{.member = member, .value = value}, .type = built};
}

// Asking a capability wrapper for what its storage holds, answering
// `reference`, a read-only reference whose dereference is that storage. An
// operation on the wrapper rather than a way of naming its storage: the
// wrapper decides what it shows, which is why a bare wrapper place -- the
// wrapper read as a value, or rebound -- stays a different program.
[[nodiscard]] inline auto MakeCellGetCallExpr(ExprId cell, TypeId reference)
    -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{.target = support::BuiltinFn::kGet, .receiver = cell},
              .arguments = {}},
      .type = reference};
}

// What a capability wrapper's storage held in the Preponed region of the
// current time slot (LRM 16.5.1). The wrapper keeps that value apart from its
// storage and answers a reference to it, as an ordinary read answers one to the
// storage, so a select into it reads only the part it names.
[[nodiscard]] inline auto MakeCellSampledGetCallExpr(
    ExprId cell, TypeId reference) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kSampledGet,
                      .receiver = cell},
              .arguments = {}},
      .type = reference};
}

// Arming a capability wrapper to answer for a sampled value (LRM 16.5.1), and
// installing the one it answers with until a later slot first changes it. Runs
// once the design's variable initializers have, so what it installs is the
// value a declaration assigned.
[[nodiscard]] inline auto MakeCellArmSamplingCallExpr(
    ExprId cell, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kArmSampling,
                      .receiver = cell},
              .arguments = {}},
      .type = void_type};
}

// Filling a history with the expression's default sampled value and fixing how
// far back it reaches (LRM 16.9.3). Runs where the design activates, for the
// same reason arming does: what the subject evaluates to there is the value
// every declaration assigned, which is what the standard makes the answer
// before the kth prior tick exists.
[[nodiscard]] inline auto MakeSampledHistoryInstallCallExpr(
    ExprId history, ExprId default_value, ExprId depth, TypeId void_type)
    -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kSampledHistoryInstall,
                      .receiver = history},
              .arguments = {default_value, depth}},
      .type = void_type};
}

// Recording what one tick of the clocking event settled (LRM 16.9.3).
[[nodiscard]] inline auto MakeSampledHistoryPushCallExpr(
    ExprId history, ExprId value, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kSampledHistoryPush,
                      .receiver = history},
              .arguments = {value}},
      .type = void_type};
}

// What the tick `ticks_back` ticks before this one settled, counting from 1 for
// the most recent (LRM 16.9.3).
[[nodiscard]] inline auto MakeSampledHistoryAtCallExpr(
    ExprId history, ExprId ticks_back, TypeId value) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kSampledHistoryAt,
                      .receiver = history},
              .arguments = {ticks_back}},
      .type = value};
}

// Installs what a capability wrapper's declaration gives it, once at
// construction. `entry` names which install this is, and `prototype` is a value
// of the declared type: the wrapper takes its representation and holds it. No
// runtime handle: it runs before any process, so there are no subscribers to
// fire.
[[nodiscard]] inline auto MakeCapabilityInstallCallExpr(
    ExprId wrapper, ExprId prototype, support::BuiltinFn entry,
    TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = entry, .receiver = wrapper},
              .arguments = {prototype}},
      .type = void_type};
}

// Installs what a net's declaration gives it, once at construction. `entry`
// names which resolution the net performs (LRM 6.6), and `arguments` are what
// the net's data type gives it -- a value of an unpacked aggregate type, and
// nothing for an integral type -- followed by the contribution the net type
// makes to the net's own resolution: the scalar the net shows where nothing
// drives it, and the strength it holds that scalar at (LRM 6.7.1, 28.11).
[[nodiscard]] inline auto MakeNetInstallCallExpr(
    ExprId net, std::vector<ExprId> arguments, support::BuiltinFn entry,
    TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = entry, .receiver = net},
              .arguments = std::move(arguments)},
      .type = void_type};
}

// `net.AttachDriver(strength)` -- attaches a driver contributing at `strength`
// to a net's resolution and yields the driver handle (the result type is the
// driver type).
[[nodiscard]] inline auto MakeNetAttachDriverCallExpr(
    ExprId net, ExprId strength, TypeId driver_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kAttachDriver,
                      .receiver = net},
              .arguments = {strength}},
      .type = driver_type};
}

// `driver.DriveFrom(object)` -- states that what a driver gives is the whole of
// `object`, a borrowed pointer to a variable's cell or to a net; `entry` says
// which of the two it is.
[[nodiscard]] inline auto MakeDriveFromCallExpr(
    ExprId driver, ExprId object, support::BuiltinFn entry, TypeId void_type)
    -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = entry, .receiver = driver},
              .arguments = {object}},
      .type = void_type};
}

// `driver.ShowSource()` -- the driver's turn at time zero, yielding whether its
// net shows the object the driver carries from here on.
[[nodiscard]] inline auto MakeShowSourceCallExpr(
    ExprId driver, TypeId bool_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kShowSource,
                      .receiver = driver},
              .arguments = {}},
      .type = bool_type};
}

// `net.Join(other, here, there, width)` -- states that `width` positions of
// `net` starting at `here`, and the same many of `other` starting at `there`,
// are one physical net whose contributions resolve together (LRM 23.3.3.7,
// 10.11). `other` is a borrowed pointer to the net on the other side, since
// what crosses is that net itself and not its value; the positions are counted
// from each value's least significant one. A connection naming a whole net on
// each side states every position of it, which is why there is no second
// shape for one. The two sides are interchangeable, with their positions:
// a connection that states this states no direction.
[[nodiscard]] inline auto MakeNetJoinCallExpr(
    ExprId net, ExprId other, ExprId here, ExprId there, ExprId width,
    TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kNetJoin, .receiver = net},
              .arguments = {other, here, there, width}},
      .type = void_type};
}

// `cell.BeginTakeover(level, first, count)` -- puts the `count` positions from
// `first` of what a cell holds, as its type numbers them, under a procedural
// continuous assignment, superseding whatever held them at that level, and
// yields the generation the new evaluation carries (LRM 10.6).
[[nodiscard]] inline auto MakeBeginTakeoverCallExpr(
    ExprId cell, ExprId level, ExprId first, ExprId count,
    TypeId generation_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kBeginTakeover,
                      .receiver = cell},
              .arguments = {level, first, count}},
      .type = generation_type};
}

// `cell.DriveTakeover(level, generation, value)` -- states what a takeover's
// evaluation produced, as a value of what the cell holds of which the
// positions the takeover holds are read, and yields whether that evaluation
// still holds any (LRM 10.6).
[[nodiscard]] inline auto MakeDriveTakeoverCallExpr(
    ExprId cell, ExprId level, ExprId generation, ExprId value,
    TypeId bool_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kDriveTakeover,
                      .receiver = cell},
              .arguments = {level, generation, value}},
      .type = bool_type};
}

// `cell.EndTakeover(level, first, count)` -- takes the `count` positions from
// `first` of what a cell holds back out of the procedural continuous
// assignment at `level` (LRM 10.6).
[[nodiscard]] inline auto MakeEndTakeoverCallExpr(
    ExprId cell, ExprId level, ExprId first, ExprId count, TypeId void_type)
    -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = support::BuiltinFn::kEndTakeover,
                      .receiver = cell},
              .arguments = {level, first, count}},
      .type = void_type};
}

// `&place` -- the address-of dual of `DerefExpr`. `pointer_type` must be
// `PointerType{ kBorrowed, pointee = <operand expr's type> }`; the caller
// supplies it so this helper need not look up the operand's type.
[[nodiscard]] inline auto MakeAddressOfExpr(ExprId operand, TypeId pointer_type)
    -> Expr {
  return Expr{.data = AddressOfExpr{.operand = operand}, .type = pointer_type};
}

}  // namespace lyra::mir

// A `CallableTarget` is a value identity, so it keys hashed containers directly
// rather than being unwrapped to its parts at the use site.
template <>
struct std::hash<lyra::mir::CallableTarget> {
  auto operator()(lyra::mir::CallableTarget target) const noexcept
      -> std::size_t {
    const std::size_t owner = std::hash<lyra::mir::ClassId>{}(target.owner);
    const std::size_t slot = std::hash<lyra::mir::CallableId>{}(target.slot);
    return owner ^ (slot << 1U);
  }
};
