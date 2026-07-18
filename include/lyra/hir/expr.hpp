#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/unary_op.hpp"

namespace lyra::hir {

struct PrimaryExpr {
  Primary data;
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

enum class AssignKind : std::uint8_t {
  kBlocking,
  kNonBlocking,
};

// `compound_op.has_value()` marks a compound assignment (`+=`, `-=`, etc.):
// the runtime reads the lvalue, combines with `rhs`, writes back -- the
// LRM 11.4.1 "evaluate target only once" rule is delegated to the backend's
// compound-op emit (the C++ proxy's `operator+=` etc.). `rhs` is already
// typed to match `lhs`; AST -> HIR inserts a `ConversionExpr` if slang's
// expansion required one. LRM A.6.2 forbids compound on non-blocking, so
// `kind == kNonBlocking && compound_op.has_value()` is an InternalError.
//
// `lhs` is an ExprId pointing at any expression whose form is addressable.
// Allowed forms: a PrimaryExpr var reference, ElementSelectExpr /
// RangeSelectExpr on an addressable base, or a ConcatExpr of addressable
// operands (the latter is the LRM 11.4.12 destructuring LHS form).
// Lvalue-ness is positional -- determined by appearance in this `lhs`
// field, not by an extra tag on the expression.
struct AssignExpr {
  AssignKind kind;
  ExprId lhs;
  std::optional<BinaryOp> compound_op = std::nullopt;
  ExprId rhs;
};

// LRM 11.4.2: `++a`, `a++`, `--a`, `a--`. Behave as blocking assignments;
// postfix yields the operand's prior value, prefix yields the new value. The
// `target` is an ExprId whose form must be addressable -- the same allowed
// forms as `AssignExpr.lhs` minus `ConcatExpr` (slang rejects `++{a,b}` at
// AST construction). Lvalue-ness is positional, validated by
// `ValidateAssignableSlangExpr` at AST -> HIR time.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

// LRM 7.12 array-method `with` clause. `id` names this clause; the element
// (`item`) and its index (`item.index`) are referenced inside `expr` by that
// identity and role (`IterationBindingRef`), so a clause nested in `expr` that
// reads an outer iterator still names the outer clause. `element_name` is the
// source iterator name, kept so the synthesized iteration closure's element
// parameter renders readably.
struct WithClause {
  WithClauseId id;
  std::string element_name;
  ExprId expr;
};

// LRM 21.3.4.4 form 2d (`$fread(mem, fd, , count)`) and any future
// system-call positional-elision case lands here as `std::nullopt` at the
// elided slot; user calls and most system calls leave every entry filled.
// Per-subroutine HIR-to-MIR lowering decides whether elision is meaningful
// at each position.
struct CallExpr {
  SubroutineRef callee;
  std::vector<std::optional<ExprId>> arguments;
  std::optional<WithClause> with_clause = std::nullopt;
};

struct InsideExpr {
  ExprId lhs;
  std::vector<InsideItem> items;
};

struct ElementSelectExpr {
  ExprId base_value;
  ExprId index;
};

struct RangeSelectExpr {
  ExprId base_value;
  RangeBounds bounds;
};

// Struct or union member access (LRM 7.2 / 7.3): `field_index` is the
// declaration-order position of the member within the aggregate arena the
// receiver's type names. The arena is uniquely determined by the receiver's
// type -- struct and union have no inheritance -- so no owner qualification
// is carried on the access.
struct MemberAccessExpr {
  ExprId base_value;
  FieldId field_index;
};

// Class property access (LRM 8.4 / 8.13): `owner` names the class that
// declares the property, and `field_index` is the slot within that class's
// property arena. Under inheritance, the receiver's runtime class may not be
// the declaring class -- the receiver reaches the object through a handle to
// a class that extends `owner` -- so identity is owner-qualified rather than
// derived from the receiver's type.
struct ClassPropertyAccessExpr {
  ExprId base_value;
  ClassId owner;
  FieldId field_index;
};

struct ConcatExpr {
  std::vector<ExprId> operands;
};

// LRM 11.4.12 / 11.4.12.2: `{multiplier{...}}` is a replication built around
// an inner concatenation. The inner ExprId always points to a ConcatExpr.
struct ReplicationExpr {
  ExprId count;
  ExprId concat;
};

// LRM 10.9 assignment pattern (positional / named / type-key / index-key /
// default forms). Slang has normalised all four forms into a flat per-element
// expression list in target declaration order (= packed MSB-first), with each
// item already wrapped in a ConversionExpression to the field / element's
// declared type. The shape covers packed targets (struct / union / packed
// array), fixed-size unpacked arrays, and dynamic arrays; HIR-to-MIR
// dispatches to the right primitive based on the resolved target type.
struct AssignmentPatternExpr {
  std::vector<ExprId> elements;
};

// LRM 10.9 replicated assignment pattern `'{count{items...}}`. `count` is
// slang-validated as a constant positive integer. `items` is the per-
// iteration expression list -- slang stores only one iteration's items with
// that iteration's per-field casts and requires the target's per-iter type
// chunks to repeat. The lowered MIR shape is `Replication(count,
// Concat(items))` for packed targets and a `Construct(canonical_default,
// ArrayLiteral{items repeated count times})` for unpacked / dynamic targets.
struct AssignmentPatternReplicationExpr {
  ExprId count;
  std::vector<ExprId> items;
};

// LRM 7.5.1 `new[N]` / `new[N](other)` dynamic array constructor. The result
// type (the dynamic array type) lives on Expr::type; `size` evaluates to a
// longint per LRM 7.5.1 (slang enforces the operand type), and `initializer`
// holds the optional `(other)` source array used for copy-with-pad-or-truncate
// per LRM 7.5.1. HIR-to-MIR lowers this to a generic construct expression
// whose argument list is `[size, element-default prototype, optional copy
// source]`; the prototype is synthesized at lowering from the element type.
struct DynamicArrayNewExpr {
  ExprId size;
  std::optional<ExprId> initializer;
};

// LRM 8.5 class object construction `new`. Allocates a new object of the named
// class and runs its constructor, yielding a handle. The class is named by its
// declaration id; `Expr::type` is the class handle type. `arguments` are the
// constructor actuals (LRM 8.7), empty for the default `new`.
struct ClassNewExpr {
  ClassId class_id;
  std::vector<ExprId> arguments;
};

// LRM 7.9.11 associative-array literal `'{index: value, ..., default: d}`. Each
// entry pairs a key expression with a value expression; the optional default is
// the persistent fallback a read of an absent key returns (LRM 7.8.6). Slang
// keeps this distinct from the positional pattern, so the key structure is
// retained here rather than flattened into an element list. HIR-to-MIR builds
// the associative value from a vector of (key, value) tuples plus the default.
struct AssociativeAssignmentPatternExpr {
  struct Entry {
    ExprId key;
    ExprId value;
  };
  std::vector<Entry> entries;
  std::optional<ExprId> default_value;
};

using ExprData = std::variant<
    PrimaryExpr, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr, IncDecExpr,
    CallExpr, ConversionExpr, InsideExpr, ElementSelectExpr, RangeSelectExpr,
    MemberAccessExpr, ClassPropertyAccessExpr, ConcatExpr, ReplicationExpr,
    AssignmentPatternExpr, AssignmentPatternReplicationExpr,
    DynamicArrayNewExpr, ClassNewExpr, AssociativeAssignmentPatternExpr>;

struct Expr {
  TypeId type;
  ExprData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
