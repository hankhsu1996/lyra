#pragma once

// Lowering of call expressions (LRM 13): user-defined subroutine calls,
// builtin-method calls (enum / string / event / array / iterator), and
// system-subroutine calls. The system-subroutine arm fans out to the
// per-family handlers under `expression/system/*.hpp`; this header is the
// single dispatch surface every call site recurses through.

#include <optional>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

// The type of the index an array method hands its callable beside each element
// of `array` (LRM 7.12.4): the key an associative array holds the element
// under, and the ordinal position for every other array.
[[nodiscard]] auto ArrayMethodIndexType(
    const mir::CompilationUnit& unit, mir::TypeId array) -> mir::TypeId;

// A call of the method `method` the language defines on a built-in type, on
// `receiver` where the method acts on a value, with `operands` in the order a
// source call writes them, all already in `block`. What its entry takes that
// no source writes -- the engine it reaches, and the default of the result's
// type for an answer to start from -- is supplied here, so a call the source
// wrote and one a lowering makes of its own are stated one way.
[[nodiscard]] auto BuildBuiltinMethodCall(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    support::BuiltinFn method, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result_type) -> mir::Expr;

// One step of an associative array's traversal (LRM 7.9.4 -- 7.9.7), as the
// expression the steps under construction in `steps` yield. `array` is the
// array's value and `index` the variable holding the index the method is asked
// about, both named in the block of `steps`. The entry `method` answers with
// the SV int 1 / 0 and the index it visited, so it completes with the two of
// them: the index reaches the variable through that variable's own write --
// which is what fires its LRM 4.3 update event -- and the answer is what the
// steps yield, at `result_type`. Binding the completion and writing the index
// back are statements while a traversal sits where an expression does (the
// canonical `do ... while (m.next(idx))` idiom, the header of a loop over the
// keys), which is why it is a sequence of steps.
[[nodiscard]] auto BuildAssociativeTraversal(
    UnitLowerer& unit_lowerer, BlockBuilder& steps, support::BuiltinFn method,
    mir::ExprId array, AccessPath index, mir::TypeId key_type,
    mir::TypeId result_type) -> mir::Expr;

// A call's meaning is independent of the enclosing scope, so one template over
// the pass class serves both the process body and the structural-scope
// (continuous-assign) contexts; explicit instantiations live in the
// implementation file. Value system subroutines on the structural path are the
// one form not yet wired.
template <ExprLowerer Lowerer>
auto LowerHirCallExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
