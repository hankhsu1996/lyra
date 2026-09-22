#pragma once

// Lowering of the LRM 6.24.2 dynamic cast, whose destination is written only
// where the assignment turns out to be valid.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The construct is a run of steps ending in the answer: the value is settled
// once, the check is made against it, the destination takes it where the
// assignment is valid, and the design is told where the source asked to be.
//
// Which check that is, is settled here rather than below: an enumeration fixes
// the values it accepts where it is declared, so the type answers, while the
// classes a handle may refer to are open across compilation units, so the
// object answers. Where the declared types already settled validity there is no
// check left and the answer is a constant.
template <ExprLowerer Lowerer>
auto LowerHirDynamicCastExpr(
    Lowerer& lowerer, const WalkFrame& frame, const hir::DynamicCastExpr& c,
    mir::TypeId result_type, diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
