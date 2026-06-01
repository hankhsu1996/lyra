#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr_id.hpp"
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

struct CallExpr {
  SubroutineRef callee;
  std::vector<ExprId> arguments;
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

// LRM 7.2.1 packed struct field access. `field_index` identifies the member
// in the base expression's PackedStructType field table; offset / width come
// from that table at every consumer site.
struct MemberAccessExpr {
  ExprId base_value;
  std::uint32_t field_index;
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

using ExprData = std::variant<
    PrimaryExpr, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr, IncDecExpr,
    CallExpr, ConversionExpr, InsideExpr, ElementSelectExpr, RangeSelectExpr,
    MemberAccessExpr, ConcatExpr, ReplicationExpr>;

struct Expr {
  TypeId type;
  ExprData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
