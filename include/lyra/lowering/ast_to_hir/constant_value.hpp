#pragma once

#include <slang/numeric/ConstantValue.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/constant_value.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

// Fold an already-evaluated slang ConstantValue into its HIR value form: a
// scalar leaf, or an unpacked aggregate's ordered component values (recursively
// folded). This is the value the metadata of a type carries -- an unpacked
// struct member default (LRM 7.2.2) -- as opposed to the expression form below
// that a constant takes when it enters an expression context. Self-contained:
// no expr arena, because a value references nothing. Packed aggregates never
// reach here -- slang folds them to a single integral value.
auto MakeConstantValue(const slang::ConstantValue& cv, diag::SourceSpan span)
    -> diag::Result<hir::ConstantValue>;

// Materialize an already-evaluated slang ConstantValue as an HIR expression of
// `type`. A scalar value (integral, real, shortreal, string) becomes a literal.
// An unpacked array becomes an AssignmentPatternExpr whose elements are
// themselves materialized constants, appended to the frame's expr arena; `unit`
// supplies the element type the recursion descends into. Packed aggregates
// never reach here -- slang folds them to a single integral value. Used
// wherever a slang-folded constant is spliced into the HIR -- parameter and
// enum-value references, and defaulted port connections.
auto MakeConstantValueExpr(
    const hir::ModuleUnit& unit, WalkFrame frame,
    const slang::ConstantValue& cv, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
