#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerProcessExprData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ProcessLoweringState& proc_state, const BodyLoweringState& body_state,
    const ScopeStack& stack, const hir::Process& hir_process,
    const hir::ExprData& data) -> mir::ExprData;

auto LowerStructuralExprData(const hir::ExprData& data)
    -> diag::Result<mir::ExprData>;

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp;

}  // namespace lyra::lowering::hir_to_mir
