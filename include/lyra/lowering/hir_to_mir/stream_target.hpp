#pragma once

// A streaming concatenation as the target of a write (LRM 11.4.14.3,
// 11.4.14.4): the stream the source makes, re-ordered as the operator says,
// cut among the members in the order the source wrote them. A variable is
// given as many bits as its type fixes; an array a `with` ranges, the elements
// the range names when its turn comes; and the first member whose width the
// running program decides, what the members after it leave room for.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

// What `stream` names as the target of a write.
template <ExprLowerer L>
[[nodiscard]] auto LowerStreamTarget(
    L& lowerer, const hir::StreamingConcatExpr& stream, WalkFrame frame)
    -> diag::Result<StreamTarget>;

// The bits `value` makes as a stream, re-ordered as `stream` says and cut
// among its members, each share handed to `sink` before the next member is
// cut.
[[nodiscard]] auto DistributeStreamTarget(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const StreamTarget& stream, mir::ExprId value, diag::SourceSpan span,
    const ShareSink& sink) -> diag::Result<void>;

}  // namespace lyra::lowering::hir_to_mir
