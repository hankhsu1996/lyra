#pragma once

#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::lowering::hir_to_mir {

auto TranslateValueDeclRefToMember(
    const UnitLoweringState& unit_state, const hir::ValueDeclRef& ref)
    -> mir::MemberId;

auto LowerPrimary(
    const UnitLoweringState& unit_state, const hir::Primary& primary)
    -> mir::ExprData;

auto LowerExprData(
    const UnitLoweringState& unit_state, const hir::ExprData& data)
    -> mir::ExprData;

}  // namespace lyra::lowering::hir_to_mir
