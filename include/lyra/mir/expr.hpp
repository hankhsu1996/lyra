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
#include "lyra/mir/runtime_diagnostic.hpp"
#include "lyra/mir/runtime_file_io.hpp"
#include "lyra/mir/runtime_finish.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_scan.hpp"
#include "lyra/mir/runtime_submit.hpp"
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
// ElementSelectExpr / RangeSelectExpr on an addressable base. The
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
// ElementSelectExpr, or RangeSelectExpr); ConcatExpr-as-target is illegal
// per slang.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;
};

struct SystemSubroutineCallee {
  support::SystemSubroutineId id;
};

using Callee = std::variant<
    SystemSubroutineCallee, StructuralSubroutineRef, BuiltinMethodCallee>;

struct CallExpr {
  Callee callee;
  std::vector<ExprId> arguments;
};

using RuntimeCall = std::variant<
    RuntimePrintCall, RuntimeDiagnosticCall, RuntimeFinishCall,
    RuntimeSubmitObservedCall, RuntimeSubmitNbaCall, RuntimeFileOpenCall,
    RuntimeFileCloseCall, RuntimeFileGetcCall, RuntimeFileUngetcCall,
    RuntimeFileGetsCall, RuntimeFileReadCall, RuntimeFileSeekCall,
    RuntimeFileRewindCall, RuntimeFileTellCall, RuntimeFileEofCall,
    RuntimeFileErrorCall, RuntimeFileFlushCall, RuntimeScanCall>;

struct RuntimeCallExpr {
  RuntimeCall call;
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

// LRM 10.9.1: simple assignment pattern `'{e1, e2, ...}`. The aggregate type
// lives on Expr::type.
struct ArrayLiteralExpr {
  std::vector<ExprId> elements;
};

using ExprData = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, StructuralParamRef,
    StructuralVarRef, ProceduralVarRef, UnaryExpr, BinaryExpr, ConditionalExpr,
    AssignExpr, IncDecExpr, CallExpr, RuntimeCallExpr, ConversionExpr,
    ClosureExpr, ElementSelectExpr, RangeSelectExpr, ConcatExpr,
    ReplicationExpr, ArrayLiteralExpr>;

struct Expr {
  ExprData data;
  TypeId type;
};

}  // namespace lyra::mir
