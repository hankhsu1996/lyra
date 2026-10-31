#pragma once

#include <expected>
#include <functional>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Evaluates a select a path element was written with, in the block a walk
// along the path is writing, as the machine integer a set of objects is asked
// by. The expression belongs to whoever holds the path, so that is who lowers
// it.
using SelectEvaluator = std::function<diag::Result<mir::ExprId>(hir::ExprId)>;

// The selects of a path `lowerer` holds, evaluated where `frame` writes.
template <typename Lowerer>
auto EvaluateSelectsIn(Lowerer& lowerer, const WalkFrame& frame)
    -> SelectEvaluator {
  return [&lowerer, frame](hir::ExprId position) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(position), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    mir::Builder block = frame.Builder();
    return BuildToInt64Call(
        lowerer.Owner().Unit(), block, block.Add(*std::move(lowered)));
  };
}

}  // namespace lyra::lowering::hir_to_mir
