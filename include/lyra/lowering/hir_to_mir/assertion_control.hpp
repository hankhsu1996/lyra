#pragma once

#include <cstdint>

#include "lyra/hir/procedural_scope.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::lowering::hir_to_mir {

// What an execution of a statement the assertion control tasks select may do,
// read off the statement's control as the execution begins and held in a local
// of `frame`'s block, since a change made while it runs does not reach it (LRM
// 20.11).
[[nodiscard]] auto ReadAllowed(
    ProcessLowerer& process, const WalkFrame& frame,
    hir::ControlledAssertionId assertion) -> mir::LocalId;

// Whether `allowed` lets the execution do what `what` names, one of the
// `support::kAssertion*` bits.
[[nodiscard]] auto Allows(
    const mir::CompilationUnit& unit, mir::Builder block, mir::LocalId allowed,
    std::uint64_t what) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
