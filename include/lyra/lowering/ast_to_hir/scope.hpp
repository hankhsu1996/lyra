#pragma once

#include <span>

#include <slang/ast/Scope.h>
#include <slang/ast/symbols/ValueSymbol.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

// home_frame is the frame where the canonical hir::LoopVarDecl lives, not
// the new scope's frame. For loop-generate body lowering, this is the loop
// generate's parent frame, so body refs compute correct hops up to it.
struct ScopeEntryLoopVarBinding {
  const slang::ast::ValueSymbol* symbol;
  ScopeFrameId home_frame;
  hir::LoopVarDeclId loop_var;
  hir::TypeId type;
};

auto LowerScopeInto(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    hir::StructuralScope& scope, const slang::ast::Scope& slang_scope,
    ScopeStack& stack,
    std::span<const ScopeEntryLoopVarBinding> entry_loop_var_bindings = {})
    -> diag::Result<void>;

}  // namespace lyra::lowering::ast_to_hir
