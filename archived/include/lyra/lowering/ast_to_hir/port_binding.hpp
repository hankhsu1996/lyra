#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/text/SourceLocation.h>

#include "lyra/common/constant.hpp"
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
// - kDriveParentToChild: input ports, parent expr -> child port
// - kDriveChildToParent: output ports (net and variable), child port ->
//   parent place via connection process. Child owns its port storage;
//   propagation to parent is connection work.
struct PortBinding {
  enum class Kind : int32_t {
    kDriveParentToChild,  // input: parent expr -> child port
    kDriveChildToParent,  // output (net + variable): child port -> parent place
  };

  Kind kind = Kind::kDriveParentToChild;

  // Topology locator: which child instance this binding targets.
  // Instance-level identity used only for child-site routing in
  // design_lower.cpp topology stitching.
  SymbolId child_instance_sym;

  // Definition-level ordinal within the child module's declared port
  // list. Stable across all instances of the same specialization.
  uint32_t child_port_ordinal = UINT32_MAX;

  SymbolId parent_instance_sym;  // Parent module's instance symbol
  SourceSpan span;

  // Parent-side source for kDriveParentToChild input ports.
  // Either a compile-time constant (ConstId) or a runtime expression
  // (ExpressionId). Determined at port-binding lowering by semantic
  // constant evaluation.
  //
  // If parent_constant_id is valid: this is a constant source.
  // Otherwise: parent_rvalue is the runtime expression.
  ConstId parent_constant_id{
      UINT32_MAX};                  // Valid for constant kDriveParentToChild
  hir::ExpressionId parent_rvalue;  // Valid for expression kDriveParentToChild
  hir::ExpressionId parent_lvalue;  // Valid for kDriveChildToParent
};

// Design-level binding plan (persists beyond AST->HIR).
// Expressions stored here reference only design symbols (no module locals)
// and live in the shared HIR arena.
struct DesignBindingPlan {
  std::vector<PortBinding> bindings;
};

}  // namespace lyra::lowering::ast_to_hir
