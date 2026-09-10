#pragma once

// Lowering of `AssignExpr` (LRM 10.4). `AssignExpr` has no structural form --
// continuous assignment is its own scope-level construct, not an expression --
// so this family is procedural only.

#include <optional>
#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirAssignExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;

// One part of a left-hand-side destructuring (LRM 11.4.12): the place it
// writes, the share of the distributed value it takes, and that share's type.
struct DestructuredPart {
  WriteTarget target;
  mir::ExprId value;
  mir::TypeId type;
};

// The deferred half of a destructuring assignment. The source wrote one
// statement, so the parts are frozen together and due at one placement, which
// is what makes a control on such an assignment read once and land every part
// in the same slot (LRM 9.4.5, 10.4.2).
auto BuildDestructuredDeferredAssign(
    ProcessLowerer& process, WalkFrame frame, diag::SourceSpan span,
    const std::optional<hir::DelayOrEventControl>& control,
    std::span<const DestructuredPart> parts) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
