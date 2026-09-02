#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/pattern.hpp"
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

// LRM 11.4.11 / 12.6.3. `conditions` is the predicate's clause sequence,
// always at least one entry; a plain `cond ? a : b` is the single
// pattern-free clause.
struct ConditionalExpr {
  std::vector<ConditionClause> conditions;
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
// target names storage, in the same forms an assignment's left side may take
// except a concatenation, which slang rejects here at AST construction.
// Whether a form names storage is decided by the position it appears in, and
// settled while lowering from the AST -- HIR holds only forms that passed.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

struct ConversionExpr {
  ConversionKind kind;
  ExprId operand;
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

// LRM 11.4.13 value range `[lo:hi]`. slang models it as an ordinary
// expression, and so does HIR: it is only meaningful as an operand of a
// membership test, but that is a rule about where it may appear, not a reason
// to give it a shape outside the expression set. Keeping it here is what lets
// every membership operand -- an `inside` item, a case-inside label -- be a
// plain `ExprId`.
struct ValueRangeExpr {
  ExprId lo;
  ExprId hi;
};

struct InsideExpr {
  ExprId lhs;
  std::vector<ExprId> items;
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
  base::ComponentIndex field_index;
};

// Class property access (LRM 8.4 / 8.13): `target` names the declaring
// class and the slot within that class's property arena. Owner-qualified
// because under inheritance the receiver's runtime class may not be the
// declaring class -- the receiver reaches the object through a handle to a
// class that extends the property owner. The external arm is used when the
// declaring class lives in another compilation unit.
struct ClassPropertyAccessExpr {
  ExprId base_value;
  ClassPropertyTarget target;
};

struct ConcatExpr {
  std::vector<ExprId> operands;
};

// LRM 11.4.12: `{multiplier{...}}` is a replication built around an inner
// concatenation. The inner ExprId always points to a ConcatExpr.
struct ReplicationExpr {
  ExprId count;
  ExprId concat;
};

// LRM 10.9 assignment pattern in the form that states every element by
// position: the list is in target declaration order -- most significant first
// where the target is packed -- with each item already converted to the field
// or element's declared type. The shape covers packed targets (struct / union /
// packed array), fixed-size unpacked arrays, and dynamic arrays; HIR-to-MIR
// dispatches to the right primitive based on the resolved target type.
//
// A pattern written with keys over an array keeps its keys instead, because a
// key names an element while this list names positions, and the two orders
// disagree for a descending dimension.
struct AssignmentPatternExpr {
  std::vector<ExprId> elements;
};

// LRM 10.9 replicated assignment pattern `'{count{items...}}`. `items` is the
// per-iteration expression list -- slang stores only one iteration's items with
// that iteration's per-field casts and requires the target's per-iter type
// chunks to repeat. Holding one iteration and a count is what stops a
// mostly-uniform aggregate costing its own length to describe; whether the
// items are expanded at all is the target type's own question.
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
// class and runs its constructor, yielding a handle. The class is named by a
// `ClassRef`: a local id when the class is declared by this unit, or a by-name
// reference against another unit's signature when the class is declared
// elsewhere. `Expr::type` is the class handle type. `arguments` are the
// constructor actuals (LRM 8.7), empty for the default `new`.
struct ClassNewExpr {
  ClassRef class_ref;
  std::vector<ExprId> arguments;
};

// LRM 11.9 tagged union expression `tagged Member primary`. `member_index` is
// the declaration-order position of the tagged member inside the union type
// (names are dropped, position is the tag). `payload` is absent when the
// member is `void`.
struct TaggedUnionExpr {
  base::ComponentIndex member_index;
  std::optional<ExprId> payload;
};

// LRM 7.9.11 associative-array literal `'{index: value, ..., default: d}`. Each
// entry pairs a key expression with a value expression; the optional default is
// the persistent fallback a read of an absent key returns (LRM 7.8.6), so it
// outlives the build rather than being spent filling elements. The keys are
// arbitrary values with no positional meaning, which is why the key structure
// is retained here rather than flattened into an element list.
struct AssociativeAssignmentPatternExpr {
  struct Entry {
    ExprId key;
    ExprId value;
  };
  std::vector<Entry> entries;
  std::optional<ExprId> default_value;
};

// LRM 10.9.1 `'{index: value, ..., default: value}` over an array. An
// `array_pattern_key` is either an index naming one element or the `default`
// key standing for every element no index named; the source may write either
// kind, and LRM 10.9.1 admits at most one `default`.
//
// The elements the default stands for are not written out. How many there are
// belongs to the target's type and appears nowhere in the source, so no count
// is carried: naming one would invent a number the source does not contain,
// and listing the elements instead would make a mostly-uniform aggregate cost
// its own length to describe -- a 32768-element array reaches the target
// language as a four-megabyte expression that no compiler will accept.
//
// A key designates an element of the target rather than computing one, the way
// a structure pattern's key names a member, so what it contributes is a
// position and not an operand. It is kept as the index it was written as rather
// than as a storage offset, because the two orders differ: offsets run from the
// dimension's left end, which is the most significant element of a packed
// array, while indices run whichever way the dimension was declared. Resolving
// one to the other is the target type's own arithmetic and belongs wherever
// that type is in hand.
struct AssignmentPatternKeyedExpr {
  struct Entry {
    std::int64_t index{};
    ExprId value;
  };
  std::vector<Entry> entries;
  std::optional<ExprId> default_value;
};

using ExprData = std::variant<
    PrimaryExpr, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr, IncDecExpr,
    CallExpr, ConversionExpr, ValueRangeExpr, InsideExpr, ElementSelectExpr,
    RangeSelectExpr, MemberAccessExpr, ClassPropertyAccessExpr, ConcatExpr,
    ReplicationExpr, AssignmentPatternExpr, AssignmentPatternReplicationExpr,
    DynamicArrayNewExpr, ClassNewExpr, AssociativeAssignmentPatternExpr,
    AssignmentPatternKeyedExpr, TaggedUnionExpr>;

struct Expr {
  TypeId type;
  ExprData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
