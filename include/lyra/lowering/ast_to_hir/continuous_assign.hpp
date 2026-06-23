#pragma once

#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

class ModuleLowerer;

// Assembles a continuous assignment (LRM 10.3.2) from its two already-built
// operand expressions and the read set its sensitivity derives from. The
// operands are interned into the frame's structural scope; the sensitivity list
// is always `ModuleLowerer::TranslateSensitivityReads(reads)` -- the single
// place a read set is mapped to its runtime projection -- so every source of a
// continuous assignment (an `assign` statement, a port connection in either
// direction) shares one type-independent sensitivity contract. The returned
// node is detached; the caller interns it into the scope.
auto BuildContinuousAssign(
    ModuleLowerer& module, WalkFrame frame, diag::SourceSpan span,
    hir::Expr lhs, hir::Expr rhs, const std::vector<SensitivityRead>& reads)
    -> hir::ContinuousAssign;

}  // namespace lyra::lowering::ast_to_hir
