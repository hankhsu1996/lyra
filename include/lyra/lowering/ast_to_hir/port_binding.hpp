#pragma once

#include <cstdint>
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

// Unified port binding with explicit rvalue/lvalue fields.
// The kind determines port connection semantics:
// - kDriveParentToChild: input ports, parent expr → child port
// - kDriveChildToParent: output variable ports, child port → parent place
// - kAlias: output/inout net ports, true identity (child aliases parent)
struct PortBinding {
  enum class Kind : int32_t {
    kDriveParentToChild,  // input: parent expr → child port
    kDriveChildToParent,  // output variable: child port → parent place
    kAlias,               // output/inout net: true identity
  };

  Kind kind = Kind::kDriveParentToChild;

  SymbolId child_port_sym;       // Child's port backing variable
  SymbolId parent_instance_sym;  // Parent module's instance symbol
  SourceSpan span;

  // Structurally explicit: different fields for different semantics.
  // INVARIANT: exactly one is valid based on kind:
  // - kDriveParentToChild: parent_rvalue is valid
  // - kDriveChildToParent, kAlias: parent_lvalue is valid
  hir::ExpressionId parent_rvalue;  // Valid for kDriveParentToChild only
  hir::ExpressionId parent_lvalue;  // Valid for kDriveChildToParent, kAlias
};

// Design-level binding plan (persists beyond AST->HIR).
// Expressions stored here reference only design symbols (no module locals)
// and live in the shared HIR arena.
struct DesignBindingPlan {
  std::vector<PortBinding> bindings;
};

}  // namespace lyra::lowering::ast_to_hir
