#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/local_ref.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"

namespace lyra::mir {

struct StringLiteral {
  std::string value;
};

// The null borrowed-pointer value. Distinct from a zero integral value: the
// type system carries the pointee identity, and C++ rejects the functional-
// cast construction (`T*()`) that the constructor primitive would otherwise
// produce for a default-init pointer.
struct NullLiteral {};

// A machine-boolean literal: a plain scalar, not a simulation value. It is what
// a runtime entry's signature or record layout takes where it wants a plain
// two-valued flag rather than an SV-typed value.
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

// Reduces an operand to a machine `bool` -- the predicate-reduction primitive.
// It stands wherever a value is consumed as a boolean: a condition context (an
// if / while / for / do-while / ternary, LRM 12.4, true when the operand is
// nonzero and false when it is zero, x, or z), an operand of a native logical
// operator (`&&` / `||` / `!`), and the inner argument of a re-shape back to a
// 1-bit packed value. The node kind, not the operand's type, is what tells a
// backend to emit the reduction, so a condition never leaves the boolean
// decision to a contextual conversion at the branch site. `Expr::type` is the
// machine boolean it yields; the operand is any value a `bool(...)` conversion
// accepts.
struct BoolCastExpr {
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

// A run of statements followed by the value the whole yields. It is the one
// node that puts a statement sequence in value position, which is how an
// evaluation of several steps stands where only an expression may: the steps
// run in order, where the expression is written, and the last one names what it
// evaluates to. Every statement form becomes value-producing by standing inside
// one, so this is the only node that lifts statements into value position.
//
// `value` names an expression of `scope`, and is always present: a run of steps
// that settles no value is a statement, which is what a block statement over
// the same scope says.
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

// `compound_op.has_value()` marks the assignment as `target op= value`;
// `nullopt` is a simple write. `value` is already typed to match `target`.
//
// `target` is either a place, whose write is a store, or a
// `ValueProjectionExpr`, whose write is a functional whole-value update through
// the designated part's owner. A join in target position (LRM 11.4.12
// destructuring LHS) is desugared upstream into a snapshot + per-part
// assignment sequence, so render does not encounter it.
struct AssignExpr {
  ExprId target;
  std::optional<BinaryOp> compound_op = std::nullopt;
  ExprId value;
};

// LRM 11.4.2: `++a`, `a++`, `--a`, `a--`. Mirrors hir::IncDecExpr. `target`
// takes the same two forms an assignment target does, a place or a designated
// part; a join in target position is illegal per slang.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

// A spelling / scope qualifier the call site provides at the point of
// invocation -- the namespace path a direct call resolves through, exactly
// the role `MyEnum::` plays in `MyEnum::first()` or `PackedArray::` in
// `PackedArray::FromInt(...)`. Distinct from the symbol's declaration owner
// (which the target's metadata knows): a qualifier is a property of this
// call, not of the symbol. A qualifier is a path in general -- a package name
// (LRM 26), or a package and then a type -- of which only the type form is
// lowered, which is why one arm carries it.
struct TypeQualifier {
  TypeId type;
};

using ScopeQualifier = std::variant<TypeQualifier>;

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

  auto operator==(const CallableTarget&) const -> bool = default;
};

// The target of a call to a method the runtime library provides for an imported
// class (LRM 9.7 `process`). A bodyless external callable whose implementation
// is a runtime symbol; the identity names the method, and the backend renders
// the call mechanically to that symbol -- no per-unit declaration and no
// per-method backend branch. A receiver, if the method has one, is `args[0]`.
struct ImportedRuntimeCallTarget {
  support::ImportedRuntimeMethod method;

  auto operator==(const ImportedRuntimeCallTarget&) const -> bool = default;
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

// Identity of a receiver-less callable owned by another compilation unit's
// namespace -- a package function or task (LRM 26.3) reached from this unit.
// The target lives outside this unit, so it carries no unit-local id: it names
// the owning unit and the callable by name, resolved against that unit's
// interface at link time, exactly as `ExternalUnitObjectType` names an
// instantiated child. A backend renders it as the free qualified form
// `unit_name::callable_name(args)`.
struct ExternalUnitCallableTarget {
  std::string unit_name;
  std::string callable_name;

  auto operator==(const ExternalUnitCallableTarget&) const -> bool = default;
};

// Identity of a class method declared by another compilation unit -- an
// instance method or a static method (LRM 8.6 / 8.10) on a class the referring
// unit reaches by name. The declaring class carries no unit-local id here, so
// the target names the declaring unit, the class's canonical (specialization)
// name, and the method's source name, resolved against that unit's signature
// at link time. A backend renders a static call as the qualified
// `unit::Class::method(args)` and an instance call as `receiver->method(args)`
// after including the declaring unit's header.
struct ExternalUnitClassMethodTarget {
  std::string unit_name;
  std::string class_name;
  std::string method_name;

  auto operator==(const ExternalUnitClassMethodTarget&) const -> bool = default;
};

// The target of a `Direct` call -- the symbol identity. The identity spaces: an
// owner-qualified callable of this unit (`CallableTarget` -- an instance method
// or a receiver-less static callable, one arena), a built-in runtime entry
// (closed-namespace `BuiltinFn`), a method the runtime library provides for an
// imported class (`ImportedRuntimeCallTarget`, LRM 9.7), a receiver-less
// callable of another compilation unit (`ExternalUnitCallableTarget`, named
// across the unit boundary), a class method of another compilation unit
// (`ExternalUnitClassMethodTarget`, class-qualified across the unit boundary),
// and a name in the DPI-C name space (`ForeignSymbolTarget`, LRM 35.4). None is
// recovered from the receiver's runtime type.
using DirectTarget = std::variant<
    CallableTarget, support::BuiltinFn, ImportedRuntimeCallTarget,
    ExternalUnitCallableTarget, ExternalUnitClassMethodTarget,
    ForeignSymbolTarget>;

// A direct call to a named symbol. The single shape for every direct
// invocation -- user method, built-in instance method, type-qualified
// static, runtime free function. The render mode (instance form
// `recv.name(rest)`, type-qualified `Q::name(args)`, or free
// `ns::name(args)`) is a fixed function of the target's signature and
// whether `qualification` is present; it is not encoded as a separate arm.
//
// Receiver, when the target's signature declares one, is `args[0]` -- an
// instance method reaches its receiver explicitly as the first argument,
// never through implicit context. Instance and static dispatch differ only in
// whether the signature has a `self` formal, not in MIR's call shape.
struct Direct {
  DirectTarget target;
  std::optional<ScopeQualifier> qualification = std::nullopt;
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

// Identity of a virtual dispatch slot introduced by a class in this
// compilation unit: the introducing class and the callable arena position
// where the slot was first declared. The receiver's dynamic type is what
// decides which implementation runs; the slot is the canonical logical
// identity a backend reads to reach the method's name and signature.
struct LocalVirtualSlot {
  ClassId owner_class;
  CallableId slot;

  auto operator==(const LocalVirtualSlot&) const -> bool = default;
};

// Identity of a virtual dispatch slot introduced by a class in another
// compilation unit. The introducing class carries no unit-local id here, so
// the slot is named by (declaring unit, class canonical name, method source
// name) -- the same triple the cross-unit override relation uses. A backend
// renders the dispatch through the target language's own virtual-call
// machinery reached by including the declaring unit's header.
struct ExternalVirtualSlot {
  std::string unit_name;
  std::string class_name;
  std::string method_name;

  auto operator==(const ExternalVirtualSlot&) const -> bool = default;
};

// The slot a virtual call names -- an intra-unit position or a cross-unit
// by-name identity. Peer of `DirectTarget`'s local / external variant
// structure: identity representation follows the compilation-unit boundary,
// never split across two dispatch node kinds.
using VirtualSlot = std::variant<LocalVirtualSlot, ExternalVirtualSlot>;

// A virtually-dispatched call: the receiver is evaluated once and then the
// implementation of the named slot on that receiver's dynamic type runs
// (LRM 8.20). The receiver rides here, distinct from user-supplied
// `CallExpr::arguments`, so the call carries exactly the arguments the SV
// source wrote and the receiver is not conflated with them.
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

// Names the place its operand stands for: the object a borrowed pointer or a
// managed handle refers to, or the storage a capability wrapper represents.
// This is place formation rather than an operation -- reading it loads the
// referent, storing into it writes the referent, a designator rooted at it
// writes part of the referent, and passing it by reference lends the referent.
// The bare operand keeps naming the pointer or the wrapper itself, so rebinding
// one is a store carrying no dereference. `Expr::type` is the referent's type.
// Taking the address of a dereferenced managed handle is how a borrowed pointer
// to a managed object is obtained.
struct DerefExpr {
  ExprId pointer;
};

// Takes the address of a place expression, yielding a borrowed pointer to
// that storage. The dual of `DerefExpr`. `operand` must be an addressable
// place (a primary value reference, a member access, a dereference, or a
// place-producing access primitive); a value expression is not addressable.
// `Expr::type` is `PointerType{ ownership = kBorrowed, pointee = operand.type
// }`. `AddressOf(Deref(p))` collapses to `p` at backend lowering when `p` is a
// borrowed pointer (the round-trip is a no-op); over a managed handle it does
// not collapse, since the address of the handle's object is not the handle.
struct AddressOfExpr {
  ExprId operand;
};

// A code address named as another function type: the erasure that puts an entry
// of one prototype into a table whose entries share a single type, and the
// restoration that calls it back at its own prototype. `Expr::type` is the
// function type the address is named as here.
//
// The two halves are one contract: an erased entry is called only after being
// restored to the exact type its definition was generated with, and both sides
// are generated from one description, so they cannot disagree. Distinct from a
// pointer cast, which retypes what an address points at rather than what
// calling it means.
struct FunctionCastExpr {
  ExprId operand;
};

// The borrowed pointer to a machine array's first element
// (`std::array::data()`, Rust `as_ptr()`). Distinct from taking the array's own
// address: this names the contiguous element storage, which is the form a
// plain-data runtime record holds a table in. `array` is a place of
// `MachineArrayType` whose storage outlives the pointer; `Expr::type` is
// `PointerType{ ownership = kBorrowed, pointee = element }`.
struct MachineArrayDataExpr {
  ExprId array;
};

// A consuming (transfer) read of the operand: the operand's contents flow
// into the enclosing expression as the last use of that operand's storage,
// and no subsequent read of the same storage is valid. `Expr::type` is the
// operand's type -- move does not change what value the expression yields,
// only how ownership crosses the boundary. Lowering emits this at last-use
// sites where the backend must transfer rather than copy (a ctor param
// forwarded to the base construction); an ordinary read stays a plain
// expression.
//
// Operand must be a type whose value can be transferred. Alias-style handle
// types carry no ownership to move, so lowering never wraps them here -- a
// move primitive over an alias is a semantic type error.
struct MoveExpr {
  ExprId operand;
};

// Re-types a reference as a reference to a different pointee type, moving no
// bits and leaving the referent untouched. `operand` is a reference-typed
// expression -- a borrowed pointer, or a handle to an object -- and
// `Expr::type` is the destination reference type of the same wrapper. Used
// where a runtime entry returns a type-erased pointer (`void*`) that the call
// site re-types, and where a handle to a subclass reaches a variable declared
// with the base class (LRM 8.14). Either way the lowering states the
// destination type in MIR so the backend never picks it from context.
struct PointerCastExpr {
  ExprId operand;
};

// Converts a machine integer to a machine integer of a different width or
// signedness. `operand` is a `MachineIntType` expression; `Expr::type` is the
// destination `MachineIntType`. This moves bits -- it truncates or extends --
// and is the primitive a foreign-call boundary crosses on: a call narrows the
// widest machine integer to its declared C carrier and widens the carrier back.
// A simulation value's resize is not this: an SV integral is a `PackedArray`
// whose resize is a library call.
struct IntCastExpr {
  ExprId operand;
};

// Identity of a class field at an access site: the class whose field arena
// declares the field, and the slot within that arena. Owner is the declaring
// class, not the receiver's class; the two coincide when the receiver's class
// declares the field itself and diverge when the field is inherited from a
// base. A backend reads the field name and type from this stated owner rather
// than deriving them from the receiver's type.
struct FieldTarget {
  ClassId owner;
  FieldId slot;

  auto operator==(const FieldTarget&) const -> bool = default;
};

// Identity of a property on an SV class another compilation unit declares. No
// unit publishes an SV class, so nothing states where such a property sits: it
// is named by (declaring unit, class canonical name, property name), matched at
// link time.
struct ExternalFieldTarget {
  std::string unit_name;
  std::string class_name;
  std::string field_name;

  auto operator==(const ExternalFieldTarget&) const -> bool = default;
};

// Which arena's field a `FieldAccessExpr` reaches. Three shapes because the
// class case is where "which arena" is a semantic decision that also splits
// on unit boundary:
//
// - `FieldTarget` (owner-qualified) is used when the receiver is a class
//   instance whose class this unit declares. The receiver's runtime class
//   type may not be the field's declaring class (inheritance), so the target
//   states both.
//
// - `ExternalFieldTarget` is used when the receiver is an instance of an SV
//   class another compilation unit declares -- that unit and the class's
//   canonical name plus the property's source name, matched at link time.
//
// - Bare `FieldId` is used when the receiver is a struct value, a closure, or
//   the object of another unit. Each carries its arena identity in its own type
//   payload (`StructType.struct_id`, `ClosureType.closure_id`,
//   `ExternalUnitObjectType.object`) and never participates in an inheritance
//   chain, so the arena is uniquely determined by the receiver's type; stating
//   it again would restate what the structural context already fixes.
using FieldRef = std::variant<FieldTarget, FieldId, ExternalFieldTarget>;

// Field access through an explicit receiver expression. `receiver` evaluates to
// a field-bearing value reached by pointer -- a class instance, a closure, or a
// promoted-scope handle (typically `LocalRef(self)` or a shared handle);
// `field` names which arena position to reach. The receiver is explicit -- a
// backend never asks "what is the current receiver?" -- and for a class
// receiver the field is owner-qualified -- a backend never derives which class
// arena to search from the receiver's type.
struct FieldAccessExpr {
  ExprId receiver;
  FieldRef field;
};

// LRM 10.9.1 array assignment pattern `'{e1, e2, ...}` element list: a value
// that is its elements and nothing more, which is what makes it a primitive.
// `Expr::type` is the list's own type -- contiguous storage of a known element
// count -- because the list is a value in its own right; a container built over
// one is a separate construction, so this is the same literal whichever one
// consumes it.
struct ArrayLiteralExpr {
  std::vector<ExprId> elements;
};

// The same value at another type that structures its bits identically --
// crossing between an enumeration and its base (LRM 6.19.3) is the case this
// arises for. Nothing is built and nothing moves; what changes is the type the
// program ascribes to the value, which is why this is a cast and not a
// construction. A destination whose representation differs is a reshape, which
// is a library call and reaches this node already reshaped.
struct ValueCastExpr {
  ExprId operand;
};

// A heterogeneous product value built from its component expressions in order
// (`TupleExpr{key, value}` is a pair). `Expr::type` is the `TupleType`, off
// which the component types are read at render time. The generic product
// literal: an associative literal is an `ArrayLiteralExpr` of these.
struct TupleExpr {
  std::vector<ExprId> components;
};

// A homogeneous sequence value built from its element expressions in order.
// `Expr::type` is the `VectorType`, off which the element type is read at
// render time. The generic sequence literal -- the homogeneous counterpart to
// `TupleExpr`, and the only way a sequence value comes into being, so a
// sequence is always fully composed at the point it is built rather than
// grown afterwards.
struct VectorExpr {
  std::vector<ExprId> elements;
};

// The suspension protocol applied to an awaitable: entering it yields control
// until the awaitable completes, then resumes with its completion value
// (LRM 9.4 timing controls, 13.5 task enable). `Expr::type` is that value's
// type -- the awaited coroutine's payload, which is a task's output pack or
// `Void` for a pure suspension (a delay, an event control, a `$finish`, a task
// with no outputs). Await is an expression, not a statement, because it is a
// value-producing operation that resumes (a suspending call), not a terminator
// like `return`: a value-yielding task completion and a void suspension are the
// same node, distinguished only by the payload type. The C++ backend realizes
// it as `co_await`.
//
// Invariant: an await appears only at statement top level -- as the expression
// of an `ExprStmt`, or as the right-hand side of the local-decl / assignment
// that binds its completion value -- never nested inside another expression,
// because SV suspends only at statement position (LRM 13.4). The node is a
// general expression for uniformity with the rest of the set; HIR-to-MIR never
// produces a nested one.
struct AwaitExpr {
  ExprId awaitable;
};

// Projects one component out of a tuple value by position: `tuple.index`. The
// inverse of `TupleExpr`, used to read a single field from a heterogeneous
// product -- a task completion's output pack, where each `output` / `inout`
// writeback reads its component. `Expr::type` is the component's type (the
// `index`-th element of the operand's `TupleType`). The C++ backend realizes it
// as `std::get<index>`.
struct TupleGetExpr {
  ExprId tuple;
  base::ComponentIndex index;
};

// Projects one element out of a sequence value by position. The inverse of
// `VectorExpr`. The position is an operand rather than part of the node
// because a sequence is homogeneous: which element is named cannot change the
// element's type, so nothing about the projection has to be known at compile
// time. Like every value-aggregate sub-access this extracts the element from
// the sequence value; a sequence of storage is reached through the indirection
// its elements already carry, not by addressing into the sequence itself.
struct VectorGetExpr {
  ExprId vector;
  ExprId index;
};

// Builds a union value whose active member is component `index`, carrying
// `value`. The value-build primitive for `UnionType`, the active-member
// analogue of `TupleExpr`: a tuple literal lists every component, a union
// literal names the one live member. Used to construct a union value -- a
// default-initialized union builds `UnionExpr{0, <member 0 default>}`.
// `Expr::type` is the `UnionType`.
struct UnionExpr {
  base::ComponentIndex index;
  ExprId value;
};

// Reads component `index` of a union value (`union.index`), the read side of
// union member access and the active-member analogue of `TupleGetExpr` (both
// `std::get<I>`-style positional access). `Expr::type` is the component's type.
// Reading an inactive member is undefined in SV (LRM 7.3) and the backend
// returns that member's default. A write is not this node's dual: writing a
// member is a descent step on the target's designator, which makes the member
// active as part of the whole-value update.
struct UnionGetExpr {
  ExprId union_value;
  base::ComponentIndex index;
};

// Builds a tagged-union value whose active tag is `tag_index`, carrying
// `payload`. The value-build primitive for `TaggedUnionType` and the tagged
// analogue of `UnionExpr`: SystemVerilog spells this as `tagged Member expr`
// (or `tagged Member` for a `void` member, LRM 11.9). Every tag carries a
// payload here, including a `void` one -- the source's missing operand is
// filled in with that element type's value at HIR-to-MIR, so a consumer never
// has to decide what an absent one would mean. `Expr::type` is the
// `TaggedUnionType`.
struct TaggedExpr {
  base::ComponentIndex tag_index;
  ExprId payload;
};

// Reads component `tag_index` of a tagged union (`u.Member`), the read side of
// tagged-union member access. Unlike `UnionGetExpr` -- which returns the
// component default on a cross-member read -- an access whose tag does not
// match the current one is a run-time error (LRM 11.9). `Expr::type` is the
// component's type; a `VoidType` component is never read (a void member has no
// payload) and so never lands here.
struct TaggedGetExpr {
  ExprId union_value;
  base::ComponentIndex tag_index;
};

// The writable location of tagged-union component `tag_index` (`u.Member` as an
// assignment target), the write side of tagged-union member access. Writing an
// untagged union's member makes that member active; a tagged union's does not
// -- a write whose `tag_index` is not the current tag is a run-time error (LRM
// 11.9), and re-tagging goes through a whole-value `TaggedExpr` construction
// rather than a member write.
struct TaggedGetRefExpr {
  ExprId union_value;
  base::ComponentIndex tag_index;
};

// Non-throwing tag check: `1` iff the tagged-union value's active tag equals
// `tag_index`. `Expr::type` is a 1-bit packed vector (the `bool`-shaped result
// that `if` and `?:` consume). Pattern-matching desugar emits this as the
// guard preceding every `TaggedGetExpr` (LRM 12.6), keeping the run-time
// mismatch error path reserved for the direct dot-access surface.
struct TaggedIsExpr {
  ExprId union_value;
  base::ComponentIndex tag_index;
};

// Used where a runtime callback surface takes a bare function value with no
// wrapper object (a lifecycle hook slot in a per-class definition constant).
// The referent is an `AbiAdapter`, never an instance method: instance
// methods have no function-pointer-compatible identity.
struct FunctionRef {
  AbiAdapterId adapter;
};

// A place naming one of this class's static constants (`Class::name`), the data
// dual of `FunctionRef`. `Expr::type` is the constant's type; as a place it is
// read in an rvalue context or has its address taken via `AddressOfExpr` (how
// the constructor passes its generated-behavior constant to the runtime base).
struct StaticConstantRef {
  StaticConstantId constant;
};

// An integral type's runtime descriptor, named by the type it describes. The
// descriptor is settled at compile time and shared by every value of that type,
// so the unit states it once and a use names which one; the type is the
// identity, so nothing about where a use appears decides what it reaches.
// `Expr::type` is the descriptor, not the integral type described.
struct PackedTypeRef {
  TypeId integral;
};

// A place naming a class's static property (`Class::name`, LRM 8.9): the
// mutable type-associated storage cell counterpart to `StaticConstantRef`.
// `owner` is the class whose static-property arena declares the cell (a
// derived source access like `Derived::inherited_prop` still names the base
// class here, mirroring the owner-qualification rule for inherited instance
// access). `Expr::type` is the property's type; as a place it appears
// wherever a `FieldAccessExpr` on a class instance would but without the
// receiver operand, and it is legal both as an rvalue and as an `AssignExpr`
// target.
struct StaticPropertyRef {
  ClassId owner;
  StaticPropertyId prop;
};

// A place naming a static variable of another compilation unit's namespace by
// name (`unit_name::variable_name`) -- a package variable (LRM 26.2) read or
// written from this unit. The reference kind for a package variable is uniform:
// a package has no instance and no receiver, so its variable is reached by name
// whether the referrer is another unit or the package's own callable, never
// through a `self`-based field access. `Expr::type` is the variable's
// observable-cell type, so a read wraps it in `Get` and a write in `Set`,
// exactly as an intra-unit signal's cell does. The storage dual of
// `ExternalUnitCallableTarget`.
struct ExternalUnitVariableRef {
  std::string unit_name;
  std::string variable_name;
};

// A static property (LRM 8.9 / 8.10) declared on a class of another
// compilation unit. Its owner has no unit-local id here; the property is
// named by (declaring unit, class canonical name, property name). A backend
// renders the access as the qualified `unit::Class::prop` after including
// the declaring unit's header.
struct ExternalStaticPropertyRef {
  std::string unit_name;
  std::string class_name;
  std::string property_name;
};

// One descent step into a value, naming a part of it. A step is positional (a
// product component, a union member) or coordinate-bearing (an element, a
// window). A coordinate-bearing step's operands are the source-level
// coordinates followed by the operands the value's family takes from its static
// type rather than from the value -- the declared range for the unpacked
// family, the declared result shape for a packed window. No operand is ever a
// rebased position: the coordinate system belongs to the value being selected.
// Each step states the type of the part it reaches, so a consumer descending
// the path knows every intermediate value's type without reimplementing the
// projection rules. The value a step descends into is the previous step's part,
// or the owner's value at the first step.
struct ComponentSelector {
  base::ComponentIndex index;
  TypeId projected_type;
};

// Selecting a union member makes it the active one; the update carries that
// activation (LRM 7.3).
struct UnionMemberSelector {
  base::ComponentIndex index;
  TypeId projected_type;
};

// LRM 7.4.5 / 7.5 / 7.8 / 7.10 / 11.5.1 positional access: an array, queue, or
// associative element, a string character, a packed bit-select.
struct ElementSelector {
  std::vector<ExprId> operands;
  TypeId projected_type;
};

// LRM 7.4.6 / 11.5.1 fixed-width window: a packed part-select, an unpacked
// slice, and a packed aggregate's member, which projects to a constant-bounds
// window over the aggregate's base.
struct SliceSelector {
  std::vector<ExprId> operands;
  TypeId projected_type;
};

using Selector = std::variant<
    ComponentSelector, UnionMemberSelector, ElementSelector, SliceSelector>;

// Designates a part of the value held by a place: the place that owns the whole
// value, and the descent that reaches the part. Writing through it is a
// functional whole-value update -- read the owner, rebuild it with the part
// replaced, store it back -- because a value aggregate's interior is not
// independently addressable. `Expr::type` is the designated part's type.
//
// The owner is a place and the path never crosses a dereference: where a chain
// re-enters storage, that dereference terminates the path and whatever
// projection reached the referent is an ordinary read inside `owner`.
//
// This is the write-side form. A read composes bottom-up through ordinary
// select expressions and needs no owner, because every step of a read is a
// function from a value to a part of it; a write has to name where the rebuilt
// value goes.
struct ValueProjectionExpr {
  ExprId owner;
  std::vector<Selector> path;
};

using ExprData = std::variant<
    StringLiteral, NullLiteral, MachineBoolLiteral, MachineIntLiteral,
    MachineFloatLiteral, LocalRef, UnaryExpr, BinaryExpr, BoolCastExpr,
    ConditionalExpr, BlockExpr, AssignExpr, IncDecExpr, CallExpr, DerefExpr,
    AddressOfExpr, MachineArrayDataExpr, MoveExpr, PointerCastExpr,
    FunctionCastExpr, IntCastExpr, FieldAccessExpr, ClosureExpr,
    ArrayLiteralExpr, ValueCastExpr, TupleExpr, VectorExpr, AwaitExpr,
    TupleGetExpr, VectorGetExpr, UnionExpr, UnionGetExpr, TaggedExpr,
    TaggedGetExpr, TaggedGetRefExpr, TaggedIsExpr, ValueProjectionExpr,
    FunctionRef, StaticConstantRef, PackedTypeRef, StaticPropertyRef,
    ExternalUnitVariableRef, ExternalStaticPropertyRef>;

struct Expr {
  ExprData data;
  TypeId type;
};

[[nodiscard]] inline auto MakeLocalRefExpr(LocalId var, TypeId type) -> Expr {
  return Expr{.data = LocalRef{.var = var}, .type = type};
}

// The library entry a call names outright, if it names one. A call whose callee
// is a callable, an indirect value, or another unit's symbol names none, so a
// consumer asking which operation this is gets nothing rather than a guess.
[[nodiscard]] inline auto DirectBuiltinFn(const CallExpr& call)
    -> std::optional<support::BuiltinFn> {
  const auto* direct = std::get_if<Direct>(&call.callee);
  if (direct == nullptr) {
    return std::nullopt;
  }
  const auto* fn = std::get_if<support::BuiltinFn>(&direct->target);
  return fn != nullptr ? std::optional{*fn} : std::nullopt;
}

[[nodiscard]] inline auto MakeFieldAccessExpr(
    ExprId receiver, FieldRef field, TypeId type) -> Expr {
  return Expr{
      .data = FieldAccessExpr{.receiver = receiver, .field = field},
      .type = type};
}

[[nodiscard]] inline auto MakeAssignExpr(
    ExprId target, ExprId value, TypeId type) -> Expr {
  return Expr{
      .data = AssignExpr{.target = target, .value = value}, .type = type};
}

// Whether the call's receiver is mutated by the dispatch. True only for a
// direct call to a built-in whose id is in the mutating set; everything
// else (direct call to a user method, indirect, construct) is false.
[[nodiscard]] auto IsMutatingCallee(const Callee& callee) -> bool;

// Whether the call hands `args[0]` back unchanged, so the call stands for
// whatever that argument stands for. A walk following where storage lives
// passes through such a call, and a consumer naming the call as a place names
// the place that argument names.
[[nodiscard]] auto IsPassThroughCallee(const Callee& callee) -> bool;

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

// `*place` -- names the storage `place` stands for. `referent_type` is what
// that storage holds: the pointee for a pointer or handle, the represented
// value type for a capability wrapper.
[[nodiscard]] inline auto MakeDerefExpr(ExprId place, TypeId referent_type)
    -> Expr {
  return Expr{.data = DerefExpr{.pointer = place}, .type = referent_type};
}

// `wrapper.Initialize(prototype)` -- fixes the declared representation (and
// default contents) once at construction. `prototype` is a value of that
// declared type; only its representation is used. No runtime handle: it runs
// before any process, so there are no subscribers to fire.
[[nodiscard]] inline auto MakeCapabilityInitializeCallExpr(
    ExprId wrapper, ExprId prototype, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kInitialize},
              .arguments = {wrapper, prototype}},
      .type = void_type};
}

// `net.AttachDriver()` -- attaches a driver to a net's resolution node and
// yields the driver handle (the result type is the driver type).
[[nodiscard]] inline auto MakeNetAttachDriverCallExpr(
    ExprId net, TypeId driver_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kAttachDriver},
              .arguments = {net}},
      .type = driver_type};
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
