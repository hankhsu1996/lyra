#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local_ref.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/struct_construct.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"

namespace lyra::mir {

enum class TimeScale : std::uint8_t { kFs, kPs, kNs, kUs, kMs, kS };

struct IntegerLiteral {
  IntegralConstant value;
};

struct StringLiteral {
  std::string value;
};

struct TimeLiteral {
  double value;
  TimeScale scale;
};

struct RealLiteral {
  double value;
};

// The null borrowed-pointer value. Distinct from `IntegerLiteral{0}`: the
// type system carries the pointee identity, and C++ rejects the functional-
// cast construction (`T*()`) that the constructor primitive would otherwise
// produce for a default-init pointer.
struct NullLiteral {};

// A machine-integer literal: a plain scalar, not a simulation value. It is what
// a runtime entry's signature takes where it wants a machine scalar (a bit
// width, a flag) rather than an SV-typed value, so no runtime value is ever
// built for it.
struct MachineIntLiteral {
  std::int64_t value;
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
// decision to a contextual conversion at the branch site. `Expr::type` carries
// the 1-bit type by convention; the operand is any value a `bool(...)`
// conversion accepts.
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

// `compound_op.has_value()` marks the assignment as `target op= value`;
// `nullopt` is a simple write. `value` is already typed to match `target`.
//
// `target` is an ExprId pointing at one of: a PrimaryExpr var reference,
// an addressable selector chain (per `mir::IsContainerAccessCall`), or a
// queue write-side access call that returns the element reference. The
// ConcatExpr-as-target form (LRM 11.4.12 destructuring LHS) is desugared
// upstream into a snapshot + per-part assignment sequence, so render does
// not encounter it.
struct AssignExpr {
  ExprId target;
  std::optional<BinaryOp> compound_op = std::nullopt;
  ExprId value;
};

// LRM 11.4.2: `++a`, `a++`, `--a`, `a--`. Mirrors hir::IncDecExpr. The
// `target` ExprId points at an addressable expression (a PrimaryExpr var
// reference or an addressable container-access chain per
// `mir::IsContainerAccessCallee`); ConcatExpr-as-target is illegal per
// slang.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

// A spelling / scope qualifier the call site provides at the point of
// invocation -- the namespace path a direct call resolves through, exactly
// the role `MyEnum::` plays in `MyEnum::first()` or `PackedArray::` in
// `PackedArray::FromInt(...)`. Distinct from the symbol's declaration owner
// (which the target's metadata knows): a qualifier is a property of this
// call, not of the symbol. Today the only arm is `TypeQualifier`; SV
// packages (LRM 26) bring `Namespace{NamespaceId}` and a `Path` composition
// for namespace-and-then-type calls.
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
// name, and the method's source name, resolved against that unit's interface
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
// owner-qualified callable of this unit (`CallableTarget` -- an instance
// method, a receiver-less static callable, or a DPI-C import, all one arena), a
// built-in runtime entry (closed-namespace `BuiltinFn`), a method the runtime
// library provides for an imported class (`ImportedRuntimeCallTarget`,
// LRM 9.7), a receiver-less callable of another compilation unit
// (`ExternalUnitCallableTarget`, named across the unit boundary), and a class
// method of another compilation unit
// (`ExternalUnitClassMethodTarget`, class-qualified across the unit boundary).
// None is recovered from the receiver's runtime type.
using DirectTarget = std::variant<
    CallableTarget, support::BuiltinFn, ImportedRuntimeCallTarget,
    ExternalUnitCallableTarget, ExternalUnitClassMethodTarget>;

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

// A call to a computed callable value -- a closure expression that
// evaluates to the callable to invoke. The indirect-call shape, the dual
// of `Direct`. Used for SV expression-position constructs whose
// evaluation has side effects (e.g., `$sscanf` writing through its output
// args while yielding the matched count).
struct Indirect {
  ExprId closure;
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

// Constructs a value of the call's result data type from the positional
// arguments -- the data type's constructor, which is just a call whose
// callee is the type itself (Python's `T(args)`, Rust's `T::new(args)`).
// The backend renders it from the result type; it carries no knowledge of
// which data type it builds. Distinct from `Direct` because it is not a
// named-symbol invocation: object/storage creation, ownership attachment,
// initialization ordering, and (future) heap allocation are the
// constructor's job, not an ordinary call's. The brace-list aggregate
// form is `ArrayLiteralExpr`, a value literal, not this.
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

// Dereferences a reference -- a borrowed pointer or a managed handle -- to
// reach the object or cell it refers to. An lvalue-producing navigation like
// ElementSelectExpr; `Expr::type` is the referent's value type. Taking the
// address of a dereferenced managed handle is how a borrowed pointer to a
// managed object is obtained.
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

// Reinterprets a borrowed pointer as a pointer to a different pointee type.
// `operand` is a pointer-typed expression; `Expr::type` is the destination
// `PointerType`. Used when a runtime entry returns a type-erased pointer
// (`void*`) that the call site re-types -- the lowering states the
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

// Identity of a class field on a class declared by another compilation unit.
// The declaring class carries no unit-local id here, so the field is named by
// (declaring unit, class canonical name, field name). A backend reads the
// field name and resolves the access through the target-language member
// lookup, exactly as it resolves any other cross-unit reference.
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
// - `ExternalFieldTarget` is used when the receiver's class lives in another
//   compilation unit -- the declaring unit and class canonical name plus the
//   field's source name, matched at link time.
//
// - Bare `FieldId` is used when the receiver is a struct value or a closure.
//   These aggregates carry their arena identity in their own type payload
//   (`StructType.struct_id`, `ClosureType.closure_id`) and never participate
//   in an inheritance chain, so the arena is uniquely determined by the
//   receiver's type; stating it again would restate what the structural
//   context already fixes.
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

// LRM 11.4.12 concatenation of packed or string operands, joined directly into
// the result value (the result shape is fully carried by `Expr::type`). An
// unpacked-queue concatenation is not this primitive: it carries an element
// shape and spread semantics and is a runtime builder call against the queue
// API, so it lowers to a `CallExpr` whose default-element and bound arguments
// come from lowering, not a payload on this node.
struct ConcatExpr {
  std::vector<ExprId> operands;
};

// LRM 11.4.12 / 11.4.12.2: `{multiplier{...}}`. `concat` always points to a
// ConcatExpr; mirrors the hir::ReplicationExpr shape.
struct ReplicationExpr {
  ExprId count;
  ExprId concat;
};

// LRM 10.9.1 array assignment pattern `'{e1, e2, ...}` element list. Feeds an
// array container's constructor; renders as `std::array<T, N>{e1, ...}` so it
// resolves uniformly against the ctor's `std::span<const T>` parameter.
// `Expr::type` is the parent container type (`UnpackedArrayType` /
// `DynamicArrayType`); the element type is read off it at render time. A value
// literal (the brace form), distinct from the constructor call it feeds.
struct ArrayLiteralExpr {
  std::vector<ExprId> elements;
};

// A heterogeneous product value built from its component expressions in order
// (`TupleExpr{key, value}` is a pair). `Expr::type` is the `TupleType`, off
// which the component types are read at render time. The generic product
// literal: an associative literal is an `ArrayLiteralExpr` of these.
struct TupleExpr {
  std::vector<ExprId> components;
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
  std::size_t index;
};

// Builds a union value whose active member is component `index`, carrying
// `value`. The value-build primitive for `UnionType`, the active-member
// analogue of `TupleExpr`: a tuple literal lists every component, a union
// literal names the one live member. Used to construct a union value -- a
// default-initialized union builds `UnionExpr{0, <member 0 default>}`.
// `Expr::type` is the `UnionType`.
struct UnionExpr {
  std::size_t index;
  ExprId value;
};

// Reads component `index` of a union value (`union.index`), the read side of
// union member access and the active-member analogue of `TupleGetExpr` (both
// `std::get<I>`-style positional access). `Expr::type` is the component's type.
// Reading an inactive member is undefined in SV (LRM 7.3) and the backend
// returns that member's default. The write side is `UnionGetRefExpr`; the read
// and write are separate access forms because a union member's read realization
// (a value) differs from its write realization (a reference that activates the
// member), exactly as a packed array element splits into element-value and
// element-reference forms.
struct UnionGetExpr {
  ExprId union_value;
  std::size_t index;
};

// The writable location of union member `index` (`u.f` as an assignment
// target), the write side and by-reference counterpart of `UnionGetExpr`.
// `Expr::type` is the component's type. As an `AssignExpr` target it carries
// `u.f = v`, `u.f op= v`, and a nested `u.f.g = v` uniformly with every other
// lvalue; the backend renders it as a reference to the active member (making
// the member active first if needed), so further member or element projection
// composes on it as on a struct member.
struct UnionGetRefExpr {
  ExprId union_value;
  std::size_t index;
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

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, NullLiteral,
    MachineIntLiteral, LocalRef, UnaryExpr, BinaryExpr, BoolCastExpr,
    ConditionalExpr, AssignExpr, IncDecExpr, CallExpr, DerefExpr, AddressOfExpr,
    MoveExpr, PointerCastExpr, IntCastExpr, FieldAccessExpr,
    StructConstructExpr, ClosureExpr, ConcatExpr, ReplicationExpr,
    ArrayLiteralExpr, TupleExpr, AwaitExpr, TupleGetExpr, UnionExpr,
    UnionGetExpr, UnionGetRefExpr, FunctionRef, StaticConstantRef,
    StaticPropertyRef, ExternalUnitVariableRef, ExternalStaticPropertyRef>;

struct Expr {
  ExprData data;
  TypeId type;
};

[[nodiscard]] inline auto MakeLocalRefExpr(LocalId var, TypeId type) -> Expr {
  return Expr{.data = LocalRef{.var = var}, .type = type};
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

// Whether the call's `args[0]` is the container being accessed (indexed or
// sliced). LHS-chain walkers use this to reach the root primary.
[[nodiscard]] auto IsContainerAccessCallee(const Callee& callee) -> bool;

// `self.Services()` -- reaches the engine facade from the scope handle.
// Every runtime-effect call threads the result as its engine handle.
[[nodiscard]] inline auto MakeServicesCallExpr(ExprId self, TypeId services)
    -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kServices},
              .arguments = {self}},
      .type = services};
}

// `cell.Get()` -- unwraps an observable cell to its inner value.
[[nodiscard]] inline auto MakeObservableGetCallExpr(
    ExprId cell, TypeId value_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kGet},
              .arguments = {cell}},
      .type = value_type};
}

// `cell.Initialize(prototype)` -- installs the cell's declared representation
// (and default contents) once at construction. `prototype` is a value of the
// cell's declared type; only its representation is used. No services: it runs
// before any process, so there are no subscribers to fire.
[[nodiscard]] inline auto MakeObservableInitializeCallExpr(
    ExprId cell, ExprId prototype, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kInitialize},
              .arguments = {cell, prototype}},
      .type = void_type};
}

// `cell.Set(services, value)` -- whole-cell write that fires subscribers
// on change.
[[nodiscard]] inline auto MakeObservableSetCallExpr(
    ExprId cell, ExprId services, ExprId value, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kSet},
              .arguments = {cell, services, value}},
      .type = void_type};
}

// `cell.Mutate(services)` -- opens a partial-write proxy. The MIR result
// type is the inner value T; consumers wrap the call in a `DerefExpr` so
// downstream selectors / operators run on T directly.
[[nodiscard]] inline auto MakeObservableMutateCallExpr(
    ExprId cell, ExprId services, TypeId value_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kMutate},
              .arguments = {cell, services}},
      .type = value_type};
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

// `driver.Update(services, value)` -- updates one driver's contribution; the
// net re-resolves and publishes on a real change.
[[nodiscard]] inline auto MakeNetDriverUpdateCallExpr(
    ExprId driver, ExprId services, ExprId value, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = Direct{.target = support::BuiltinFn::kUpdateDriver},
              .arguments = {driver, services, value}},
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
