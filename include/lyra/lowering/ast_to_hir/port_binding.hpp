#pragma once

#include <unordered_map>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/text/SourceLocation.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

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

// Input port: parent rvalue expression drives child port variable.
// These are applied at HIR->MIR as synthetic always_comb processes.
struct DriveBinding {
  SymbolId child_port_sym;   // Child's port backing variable
  hir::ExpressionId rvalue;  // Parent's rvalue expression
  SourceSpan span;
  SymbolId
      parent_instance_sym;  // Parent module's instance symbol (stable identity)
};

// Output/inout port: child port aliases parent's lvalue place.
// These are applied at HIR->MIR as alias_map entries.
struct AliasBinding {
  SymbolId child_port_sym;   // Child's port backing variable
  hir::ExpressionId lvalue;  // Parent's lvalue expression (must be a place)
  SourceSpan span;
};

// Design-level binding plan (persists beyond AST->HIR).
// Expressions stored here reference only design symbols (no module locals)
// and live in the shared HIR arena.
struct DesignBindingPlan {
  std::vector<DriveBinding> drives;
  std::vector<AliasBinding> aliases;
};

}  // namespace lyra::lowering::ast_to_hir
