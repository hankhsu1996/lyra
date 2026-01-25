#pragma once

#include <unordered_map>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/text/SourceLocation.h>

namespace lyra::lowering::ast_to_hir {

// Transient descriptor for one input port binding.
// LIFETIME: Valid only during AST->HIR lowering (same Compilation lifetime).
// Do NOT store in LoweringResult or any structure that outlives LowerDesign.
struct InputPortBinding {
  // Child's port backing variable (PortSymbol::internalSymbol)
  const slang::ast::VariableSymbol* target_var = nullptr;

  // Connection expression from parent scope (PortConnection::getExpression())
  const slang::ast::Expression* value_expr = nullptr;

  // Port connection site for error reporting (prefer over expr->sourceRange)
  slang::SourceRange connection_range;
};

// Transient map: parent instance -> bindings for its child instance ports.
// LIFETIME: Built and consumed within single LowerDesign call.
using TransientPortBindingPlan = std::unordered_map<
    const slang::ast::InstanceSymbol*, std::vector<InputPortBinding>>;

}  // namespace lyra::lowering::ast_to_hir
