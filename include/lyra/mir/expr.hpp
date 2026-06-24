#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/method_ref.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/mir/value_ref.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

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

struct UnaryExpr {
  UnaryOp op;
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
// `mir::IsContainerAccessCall`); ConcatExpr-as-target is illegal per
// slang.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

struct SystemSubroutineCallee {
  support::SystemSubroutineId id;
};

// Calls a compiler-recognized runtime entry whose identity is a closed-
// namespace `support::BuiltinFn`. Instance form: `args[0]` is the value
// receiver; remaining args are the SV arguments.
struct BuiltinFnCallee {
  support::BuiltinFn id;
};

// Type-namespace-qualified static call (e.g. `MyEnum::first()`). No value
// receiver in `args`; `type_qual` is part of the symbol identity and rides
// on the callee. Backends consume the qualifier directly.
struct BuiltinStaticCallee {
  support::BuiltinFn id;
  TypeId type_qual;
};

// A free function (no receiver, no SV-type qualifier). Distinct from
// `BuiltinStaticCallee` whose qualifier names an SV type.
struct FreeFnCallee {
  support::BuiltinFn id;
};

// Calls a closure stored as an expression in the same block.
// The backend renders the call as `(closure_lambda)(args)` -- the standard
// IIFE shape when invoked synchronously. Used for SV expression-position
// constructs whose evaluation has side effects (e.g., `$sscanf` writing
// through its output args while yielding the matched count).
struct ClosureRef {
  ExprId closure{};
};

// Constructs a value of the call's result data type from the positional
// arguments -- the data type's constructor, which is just a call whose callee
// is the type itself (Python's `T(args)`, Rust's `T::new(args)`). The backend
// renders it as `<TypeName>(args)`, the type name from the result type; it
// carries no knowledge of which data type it builds (a reference from a cell, a
// runtime-sized container from a size, a default value from no arguments). The
// brace-list aggregate form is `ArrayLiteralExpr`, a value literal, not this.
struct ConstructorCallee {};

using Callee = std::variant<
    SystemSubroutineCallee, MethodRef, BuiltinFnCallee, BuiltinStaticCallee,
    FreeFnCallee, ClosureRef, ConstructorCallee>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

// Dereferences a borrowed pointer to reach the cell it refers to. An
// lvalue-producing navigation like ElementSelectExpr; `Expr::type` is the
// pointee value type.
struct DerefExpr {
  ExprId pointer;
};

// Takes the address of a place expression, yielding a borrowed pointer to
// that storage. The dual of `DerefExpr`. `operand` must be an addressable
// place (a primary value reference, a member access, a dereference, or a
// place-producing access primitive); a value expression is not addressable.
// `Expr::type` is `PointerType{ ownership = kBorrowed, pointee = operand.type
// }`. `AddressOf(Deref(p))` collapses to `p` at backend lowering.
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

// Class-member access through an explicit receiver expression. `receiver`
// evaluates to a class-instance value (typically `LocalRef(self)`);
// `member` names which member of the receiver's class to reach. The
// receiver is explicit -- a backend never asks "what is the current receiver?".
struct MemberAccessExpr {
  ExprId receiver;
  MemberRef member;
};

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

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, NullLiteral,
    ParamRef, LocalRef, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr,
    IncDecExpr, CallExpr, DerefExpr, AddressOfExpr, PointerCastExpr,
    MemberAccessExpr, ConversionExpr, ClosureExpr, ConcatExpr, ReplicationExpr,
    ArrayLiteralExpr, TupleExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

[[nodiscard]] inline auto MakeLocalRefExpr(
    BlockHops hops, LocalId var, TypeId type) -> Expr {
  return Expr{.data = LocalRef{.hops = hops, .var = var}, .type = type};
}

[[nodiscard]] inline auto MakeMemberAccessExpr(
    ExprId receiver, MemberRef member, TypeId type) -> Expr {
  return Expr{
      .data = MemberAccessExpr{.receiver = receiver, .member = member},
      .type = type};
}

[[nodiscard]] inline auto MakeAssignExpr(
    ExprId target, ExprId value, TypeId type) -> Expr {
  return Expr{
      .data = AssignExpr{.target = target, .value = value}, .type = type};
}

// Whether the call's receiver is mutated by the dispatch. Dispatches across
// the two builtin callee shapes; non-builtin callees return false.
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
              .callee = BuiltinFnCallee{.id = support::BuiltinFn::kServices},
              .arguments = {self}},
      .type = services};
}

// `cell.Get()` -- unwraps an observable cell to its inner value.
[[nodiscard]] inline auto MakeObservableGetCallExpr(
    ExprId cell, TypeId value_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = BuiltinFnCallee{.id = support::BuiltinFn::kGet},
              .arguments = {cell}},
      .type = value_type};
}

// `cell.Set(services, value)` -- whole-cell write that fires subscribers
// on change.
[[nodiscard]] inline auto MakeObservableSetCallExpr(
    ExprId cell, ExprId services, ExprId value, TypeId void_type) -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee = BuiltinFnCallee{.id = support::BuiltinFn::kSet},
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
              .callee = BuiltinFnCallee{.id = support::BuiltinFn::kMutate},
              .arguments = {cell, services}},
      .type = value_type};
}

// `&place` -- the address-of dual of `DerefExpr`. `pointer_type` must be
// `PointerType{ kBorrowed, pointee = <operand expr's type> }`; the caller
// supplies it so this helper need not look up the operand's type.
[[nodiscard]] inline auto MakeAddressOfExpr(ExprId operand, TypeId pointer_type)
    -> Expr {
  return Expr{.data = AddressOfExpr{.operand = operand}, .type = pointer_type};
}

}  // namespace lyra::mir
