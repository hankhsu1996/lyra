#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/support/system_subroutine.hpp"
#include "lyra/value/format.hpp"

namespace lyra::mir {
struct CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

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
    -> diag::Result<std::vector<mir::RuntimePrintItem>>;

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

// Materializes a runtime print-item list into MIR: each item is constructed as
// its runtime-library type -- a literal item or a value item -- and the
// sequence becomes an array literal of those. The element subtrees are
// interned into `block`; the returned
// array root is left for the caller to intern. `time_unit_power` scales a %t
// directive (LRM 21.2.1.3) and is unread when no item carries a kTime spec.
// The canonical item-array builder every print-item-bearing effect reuses.
auto BuildPrintItemsArray(
    mir::CompilationUnit& unit, mir::Block& block,
    const std::vector<mir::RuntimePrintItem>& items,
    std::int64_t time_unit_power) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
