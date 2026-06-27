#pragma once

#include <slang/numeric/SVInt.h>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::lowering::ast_to_hir {

// Convert a slang SVInt into Lyra's canonical IntegralConstant. Performs the
// X/Z plane swap once at this boundary: slang encodes X=(value=0,unknown=1)
// and Z=(value=1,unknown=1); Lyra encodes Z=(value=0,state=1) and
// X=(value=1,state=1).
auto LowerSVIntToIntegralConstant(const slang::SVInt& sv)
    -> hir::IntegralConstant;

// Wrap a folded HIR integral constant as an integer-literal expression of
// `type`. The single place the integer-literal expression shape is built,
// reached from a slang `SVInt` (after folding) or from an already-folded
// `IntegralConstant`.
auto MakeIntegerLiteralFromConstant(
    hir::IntegralConstant value, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr;

// Materialize an already-evaluated slang integral constant as an HIR
// integer-literal expression of `type`. Shared by every site that splices a
// constant slang has folded into the HIR: parameter and enum-value references,
// and defaulted port connections.
auto MakeIntegralLiteralExpr(
    const slang::SVInt& sv, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr;

}  // namespace lyra::lowering::ast_to_hir
