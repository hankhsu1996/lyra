#pragma once

#include <slang/numeric/ConstantValue.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::lowering::ast_to_hir {

// Materialize an already-evaluated slang ConstantValue as an HIR literal
// expression of `type`: integral, real, shortreal, or string. Used wherever a
// slang-folded constant is spliced into the HIR -- parameter and enum-value
// references, and defaulted port connections. Aggregate constants are not yet
// supported.
auto MakeConstantValueExpr(
    const slang::ConstantValue& cv, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
