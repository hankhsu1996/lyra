#pragma once

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

class StructuralScopeLowerer;

// The net positions a net lvalue names (LRM 10.11 `net_lvalue`). A name with a
// constant select names one operand's; a concatenation names its operands', in
// the order written. `eval_scope` is the symbol the selects are folded against,
// and `code` is the construct's own diagnostic code, since what reaches this is
// a port connection or an alias and a refusal belongs to whichever one it is.
auto NetPositionsOfLvalue(
    StructuralScopeLowerer& scope, const slang::ast::Symbol& eval_scope,
    const slang::ast::Expression& expr, diag::SourceSpan span,
    diag::DiagCode code, WalkFrame frame) -> diag::Result<hir::NetSide>;

}  // namespace lyra::lowering::ast_to_hir
