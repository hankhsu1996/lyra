#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/support/system_subroutine.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/format_parse.hpp"

namespace lyra::mir {
struct CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

struct FormatSpec {
  value::FormatKind kind;
  value::FormatModifiers modifiers;

  FormatSpec(value::FormatKind k, value::FormatModifiers m)
      : kind(k), modifiers(m) {
  }
};

struct RuntimePrintLiteral {
  std::string text;
};

struct RuntimePrintValue {
  mir::ExprId value;
  FormatSpec spec;

  RuntimePrintValue(mir::ExprId v, FormatSpec s) : value(v), spec(s) {
  }
};

// One print item as the lowering works it out, before it is built: text, or a
// value and the conversion that formats it (LRM 21.2.1).
using RuntimePrintItem = std::variant<RuntimePrintLiteral, RuntimePrintValue>;

// Maps a system subroutine's bare-argument default radix (LRM 21.2.1.1:
// `$displayb` -> binary, `$displayh` -> hex, etc.) to the runtime
// FormatKind that drives single-argument format dispatch.
auto RadixToFormatKind(support::PrintRadix r) -> value::FormatKind;

// The print items of a list of arguments as the display tasks read one (LRM
// 21.2.1), from the argument at `arg_offset` on: the arguments contribute in
// the order written with nothing between them. A string literal contributes
// its text, each format specification in it formatting an argument after it;
// an expression no specification took is formatted in `default_radix` (LRM
// 21.2.1.1); an empty argument is a single space.
//
// `arg_offset` passes over what a task takes before its list: the descriptor
// of a file-output task, the output variable of `$swrite`, the finish number
// of `$fatal`.
template <ExprLowerer Lowerer>
auto BuildDisplayListPrintItems(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    support::PrintRadix default_radix, std::size_t arg_offset)
    -> diag::Result<std::vector<RuntimePrintItem>>;

// The text, an SV `string`, of a call that reads the argument at `arg_offset`,
// and no other, as a format string (LRM 21.3.3), each argument after it
// formatted by the directive that takes it. A literal format string taking
// every one of them is bound to them now. Any other is parsed and bound at
// simulation time: one whose text is known only then, and a literal that
// leaves arguments over, for which the clause asks a warning and that
// execution continue -- what that parse does with a surplus.
template <ExprLowerer Lowerer>
auto BuildFormatStringTextExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    std::size_t arg_offset) -> diag::Result<mir::Expr>;

// The print items as MIR, an element list of print items each constructed
// over a value and its specification or made from text. The elements are
// interned into `block`, and the list is left for the caller to intern.
// `time_unit_power` scales a %t directive (LRM 21.2.1.3) and is unread when no
// item carries one.
auto BuildPrintItemsArray(
    mir::CompilationUnit& unit, mir::Block& block,
    const std::vector<RuntimePrintItem>& items, std::int64_t time_unit_power)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
