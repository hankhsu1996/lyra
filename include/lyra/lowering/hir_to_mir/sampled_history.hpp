#pragma once

#include <optional>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// A process keeping one history, for the scope to start with the others it
// runs, and where the call it keeps the history for is written.
struct HistorySampler {
  mir::CallableId body;
  diag::SourceSpan written_at;
};

// Keeps what every sampled value function `reader` lowers declares it keeps
// (LRM 16.9.3): a process of the instance per function, which at each tick of
// its clock takes each kept piece's sampled value and records it in the
// Postponed region of that step, so a reader in the step never sees that tick;
// and, where the design activates, each history filled with what the piece is
// before any tick, which is its default sampled value (LRM 16.5.1). The
// sampling those pieces read is armed before this runs.
template <ExprLowerer Lowerer>
[[nodiscard]] auto KeepSampledHistories(
    Lowerer& reader, mir::Class& mir_class, const WalkFrame& ctor_frame,
    const WalkFrame& activate_frame)
    -> diag::Result<std::vector<HistorySampler>>;

// Where `expr` is a piece the outermost of `frame`'s prior ticks keeps, read
// through the rest, what that history holds of it; nothing elsewhere.
template <ExprLowerer Lowerer>
[[nodiscard]] auto ReadOfKeptPiece(
    Lowerer& lowerer, const hir::Expr& expr, const WalkFrame& frame)
    -> std::optional<diag::Result<mir::Expr>>;

// `$past`: the operand where the call stands, read as of the tick the call
// names (LRM 16.9.3).
template <ExprLowerer Lowerer>
[[nodiscard]] auto LowerPastValueCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::PastValueRef& ref) -> diag::Result<mir::Expr>;

// A value change function: the sampled value of this time step -- the one
// `$sampled` gives, read here the same way -- against the one of the most
// recent strictly prior tick, because the standard defines a change as one
// against the other (LRM 16.9.3).
template <ExprLowerer Lowerer>
[[nodiscard]] auto LowerValueChangeCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::ValueChangeRef& ref, mir::TypeId result_type,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
