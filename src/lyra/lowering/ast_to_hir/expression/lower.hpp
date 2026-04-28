#pragma once

#include <optional>
#include <string_view>

#include <slang/ast/Expression.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr>;

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr>;

// Short-lived state for lowering one loop-generate header's initial / stop
// / iter expressions. The synthetic loop-variable identity (and its type)
// is captured lazily on first reference, since slang exposes the
// iteration-variable's type through the `VariableSymbol` it fabricates for
// header expressions, not through the canonical genvar declaration symbol.
struct LoopHeaderState {
  std::string_view expected_name;
  const slang::ast::VariableSymbol* synthetic_symbol = nullptr;
  std::optional<hir::LoopVarDeclId> loop_var_id;
};

auto LowerLoopHeaderExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    LoopHeaderState& loop_state, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
