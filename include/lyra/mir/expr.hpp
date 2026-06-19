#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builtin_method.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/inc_dec_op.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_scan.hpp"
#include "lyra/mir/runtime_sformat.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/runtime_timescale.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_subroutine_ref.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/mir/value_ref.hpp"
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
// ElementSelectExpr / RangeSelectExpr on an addressable base, or a queue
// access method call (`WriteRef`) that returns the element reference. The
// ConcatExpr-as-target form (LRM 11.4.12 destructuring LHS) is desugared
// upstream into a snapshot + per-part assignment sequence, so render does
// not encounter it.
struct AssignExpr {
  ExprId target;
  std::optional<BinaryOp> compound_op = std::nullopt;
  ExprId value;
};

// LRM 11.4.2: `++a`, `a++`, `--a`, `a--`. Mirrors hir::IncDecExpr. The
// `target` ExprId points at an addressable expression (PrimaryExpr var ref,
// ElementSelectExpr, RangeSelectExpr, or a queue access method call returning
// an element reference); ConcatExpr-as-target is illegal per slang.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

struct SystemSubroutineCallee {
  support::SystemSubroutineId id;
};

// Calls a closure stored as an expression in the same procedural scope.
// The backend renders the call as `(closure_lambda)(args)` -- the standard
// IIFE shape when invoked synchronously. Used for SV expression-position
// constructs whose evaluation has side effects (e.g., `$sscanf` writing
// through its output args while yielding the matched count).
struct ClosureRef {
  ExprId closure{};
};

enum class RuntimeFn : std::uint8_t {
  kGetChild,
  kGetSignal,
  kRegisterSignal,
};

// Calls a runtime by-name interface method on a scope handle. `fn` selects the
// method; `name` is the compile-time signal / child name. Argument 0 is the
// scope handle the method is invoked on:
//   - `kGetChild`: the remaining arguments are the per-dimension array indices
//     (none for a scalar), each a runtime expression; renders
//     `<scope>->GetChild(name, {indices})`.
//   - `kGetSignal`: renders `<scope>->GetSignal(name)`.
//   - `kRegisterSignal`: argument 1 is the signal var whose address is
//     registered under `name`; renders `<scope>->RegisterSignal(name, &<var>)`.
//     A scope records its own by-name interface during construction with this.
// The name and indices are never bundled into one struct.
struct RuntimeNavCallee {
  RuntimeFn fn;
  std::string name;
};

using Callee = std::variant<
    SystemSubroutineCallee, StructuralSubroutineRef, BuiltinMethodCallee,
    ClosureRef, RuntimeNavCallee>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

using RuntimeCall = std::variant<
    RuntimePrintCall, RuntimeSubmitObservedCall, RuntimeSubmitNbaCall,
    RuntimeSubmitPostponedCall, RuntimeScanCall, RuntimeSFormatCall,
    RuntimeSetTimeFormatCall, RuntimePrintTimescaleCall>;

struct RuntimeCallExpr {
  RuntimeCall call;
};

// Dereferences a borrowed pointer to reach the cell it refers to. An
// lvalue-producing navigation like ElementSelectExpr; `Expr::type` is the
// pointee value type.
struct DerefExpr {
  ExprId pointer;
};

// Class-member access through an explicit receiver expression. `receiver`
// evaluates to a class-instance value (typically `ProceduralVarRef(self)`);
// `member` names which structural var of the receiver's class to reach. The
// receiver is explicit -- a backend never asks "what is the current receiver?"
// (mir.md invariant 11).
struct MemberAccessExpr {
  ExprId receiver;
  StructuralVarRef member;
};

struct ElementSelectExpr {
  ExprId base_value;
  ExprId index;
};

// Contiguous slice of `count` outer elements starting at `offset_expr`.
// HIR's three source-faithful forms (`m:n`, `i +: w`, `i -: w`) collapse to
// this single shape at HIR -> MIR; `count` is the LRM 7.4.5 compile-time
// constant width, `offset_expr` is the lsb in outer-element units.
//
// LRM 7.2.1 packed struct field access has no MIR-level distinct shape: it
// lowers at HIR -> MIR into this same slice form (since "a packed structure
// can be selected as if it were a packed array", LRM 7.2.1). A future MIR
// node for *unpacked* struct named access is a genuinely different shape (it
// maps to C++ `s.member`, not bit math) and will get its own variant when
// that work lands.
struct RangeSelectExpr {
  ExprId base_value;
  ExprId offset_expr;
  std::uint32_t count;
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

// LRM 10.9.1 array assignment pattern `'{e1, e2, ...}` element list. Always
// appears as an argument of a `ConstructExpr` whose result is an array
// container; renders as `std::array<T, N>{e1, ...}` so the surrounding ctor
// resolves uniformly against a `std::span<const T>` parameter. `Expr::type`
// is the parent container type (`UnpackedArrayType` / `DynamicArrayType`);
// the element type is read off it at render time.
struct ArrayLiteralExpr {
  std::vector<ExprId> elements;
};

// Invokes the result type's constructor with positional arguments; the result
// type lives on Expr::type. Used for runtime-sized container construction --
// LRM 7.5.1 dynamic-array `new[N]` / `new[N](other)`, and the LRM 7.6
// dynamic-array whole-array assignment that desugars to the same shape.
// Distinct from ArrayLiteralExpr in semantics: args are constructor
// arguments, not initializer-list elements, so the brace-vs-paren overload
// distinction is preserved at the C++ surface.
struct ConstructExpr {
  std::vector<ExprId> args;
};

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, StructuralParamRef,
    ProceduralVarRef, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr,
    IncDecExpr, CallExpr, RuntimeCallExpr, DerefExpr, MemberAccessExpr,
    ConversionExpr, ClosureExpr, ElementSelectExpr, RangeSelectExpr, ConcatExpr,
    ReplicationExpr, ArrayLiteralExpr, ConstructExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

[[nodiscard]] inline auto MakeProceduralVarRefExpr(
    ProceduralHops hops, ProceduralVarId var, TypeId type) -> Expr {
  return Expr{.data = ProceduralVarRef{.hops = hops, .var = var}, .type = type};
}

[[nodiscard]] inline auto MakeMemberAccessExpr(
    ExprId receiver, StructuralVarRef member, TypeId type) -> Expr {
  return Expr{
      .data = MemberAccessExpr{.receiver = receiver, .member = member},
      .type = type};
}

[[nodiscard]] inline auto MakeAssignExpr(
    ExprId target, ExprId value, TypeId type) -> Expr {
  return Expr{
      .data = AssignExpr{.target = target, .value = value}, .type = type};
}

// `self.Services()` -- reaches the engine facade from the scope handle.
// `self` is the receiver ExprId; `services` is ServicesType. The caller does
// the AddExpr. Every runtime-effect call threads the result as its engine
// handle (docs/decisions/runtime-effects-as-generic-calls.md).
[[nodiscard]] inline auto MakeServicesCallExpr(ExprId self, TypeId services)
    -> Expr {
  return Expr{
      .data =
          CallExpr{
              .callee =
                  BuiltinMethodCallee{
                      .method =
                          ScopeMethodInfo{.kind = ScopeMethodKind::kServices}},
              .arguments = {self}},
      .type = services};
}

}  // namespace lyra::mir
