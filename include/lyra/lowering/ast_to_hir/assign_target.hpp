#pragma once

#include <optional>

#include <slang/ast/Expression.h>

#include "lyra/hir/assign_target.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

// Canonical LHS binder - the single source of truth for assignment target
// lowering. This function:
// 1. Enumerates LHS expression kinds
// 2. Produces a closed set of AssignTarget forms
// 3. Emits shape/legality diagnostics for unsupported forms
//
// Returns nullopt on error (diagnostic already emitted).
auto LowerAssignTarget(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> std::optional<hir::AssignTarget>;

}  // namespace lyra::lowering::ast_to_hir
