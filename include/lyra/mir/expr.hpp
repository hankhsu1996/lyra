#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/cast.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local_ref.hpp"
#include "lyra/mir/method_id.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/struct_construct.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

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

// A host-language integer literal: renders as a bare C++ integer (e.g.
// `42LL`), not as a `PackedArray::Int(42)` runtime value. Used at the
// boundary between MIR and the runtime when a runtime entry's signature
// takes a host scalar (`uint64_t` bit-width, `bool` flag) rather than a
// SV-typed value. `Expr::type` is conventionally `builtins.int32` so
// downstream type accounting stays uniform; the host-int-ness is implicit
// from the node kind.
struct HostIntLiteral {
  std::int64_t value;
};

struct UnaryExpr {
  UnaryOp op;
  ExprId operand;
};

// Reduces an operand to a host `bool`. Used to bring real / string operands
// onto the host-bool plane before a C++ native logical operator (`&&`, `||`,
// `!`), and as the inner argument of `BuiltinFn::kFromBool` when the result
// must re-shape to a 1-bit packed value. `Expr::type` is conventionally
// `builtins.bit1` (the result is observable only after `FromBool` re-shapes
// it; the operand of a `BoolCastExpr` is whatever the host's `bool(...)`
// conversion accepts).
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

// The target of a `Direct` call -- the symbol identity. Two identity
// spaces today: built-in runtime entries (closed-namespace `BuiltinFn`)
// and user-declared class methods (per-class `MethodId` arena). The two
// collapse into one `CallableId` once external callables (DPI) and SV
// class statics share one callable-declaration shape.
using DirectTarget = std::variant<MethodId, support::BuiltinFn>;

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

using Callee = std::variant<Direct, Indirect, Construct>;

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

// Reinterprets a borrowed pointer as a pointer to a different pointee type.
// `operand` is a pointer-typed expression; `Expr::type` is the destination
// `PointerType`. Used when a runtime entry returns a type-erased pointer
// (`void*`) that the call site re-types -- the lowering states the
// destination type in MIR so the backend never picks it from context.
struct PointerCastExpr {
  ExprId operand;
};

// Field access through an explicit receiver expression. `receiver` evaluates to
// a field-bearing value reached by pointer -- a class instance, a closure, or a
// promoted-scope handle (typically `LocalRef(self)` or a shared handle);
// `field` names which field of the receiver's aggregate to reach. The receiver
// is explicit -- a backend never asks "what is the current receiver?".
struct FieldAccessExpr {
  ExprId receiver;
  FieldId field;
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

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, NullLiteral,
    HostIntLiteral, ParamRef, LocalRef, UnaryExpr, BinaryExpr, BoolCastExpr,
    ConditionalExpr, AssignExpr, IncDecExpr, CallExpr, DerefExpr, AddressOfExpr,
    PointerCastExpr, CastExpr, FieldAccessExpr, StructConstructExpr,
    ClosureExpr, ConcatExpr, ReplicationExpr, ArrayLiteralExpr, TupleExpr,
    AwaitExpr, TupleGetExpr, UnionExpr, UnionGetExpr, UnionGetRefExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

[[nodiscard]] inline auto MakeLocalRefExpr(LocalId var, TypeId type) -> Expr {
  return Expr{.data = LocalRef{.var = var}, .type = type};
}

[[nodiscard]] inline auto MakeFieldAccessExpr(
    ExprId receiver, FieldId field, TypeId type) -> Expr {
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
