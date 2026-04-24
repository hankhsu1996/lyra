#pragma once

#include "lyra/hir/expr.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerExprData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::ExprData& data) -> mir::ExprData;

auto LowerPrimary(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::Primary& p) -> mir::ExprData;

auto ResolveVarRef(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::VarDeclRef& ref) -> mir::MemberId;

auto TranslateLvalueTarget(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::Lvalue& lvalue) -> mir::MemberId;

}  // namespace lyra::lowering::hir_to_mir
