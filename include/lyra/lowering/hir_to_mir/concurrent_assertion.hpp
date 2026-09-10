#pragma once

#include <cstdint>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// The class field holding one assertion's attempts, as the place every
// operation on them acts through. It needs no route: an assertion is evaluated
// on the scope that declares it, so the storage and everything that reaches it
// are always the same scope.
[[nodiscard]] auto BuildEvaluationAttemptsExpr(
    mir::Block& block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, hir::ConcurrentAssertionId id)
    -> mir::ExprId;

// What the design's activation has to do with the callables one concurrent
// assertion lowered to.
struct InstalledConcurrentAssertion {
  // The processes the design has to register for this assertion: the one that
  // advances it at every tick of its clock, and -- where the source wrote a
  // `disable iff` -- the one watching that condition between ticks.
  std::vector<mir::CallableId> processes;
  mir::CallableId pass_action;
  mir::CallableId fail_action;
  // How wide a position set of this assertion's automaton is.
  std::uint32_t words = 0;
  // What an attempt still in flight when the run ends is owed: an obligation is
  // met by "holds", a coverage goal needed a match (LRM 16.12.2).
  bool pending_holds = true;
};

// Lowers one concurrent assertion the scope declares into the callables it
// takes -- the statements each outcome selects, the advance one tick performs,
// and the process that submits that advance to Observed at every tick of the
// clock -- adding each to `mir_class`.
[[nodiscard]] auto LowerConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const DeclaredScopes& scopes,
    hir::ConcurrentAssertionId id, const hir::ConcurrentAssertionDecl& decl)
    -> diag::Result<InstalledConcurrentAssertion>;

// Fills the storage where the design activates: the width of a position set,
// the answer a pending attempt is owed, and the statements an outcome selects.
// Nothing about an assertion needs an earlier phase -- what it reads is reached
// through cells sealed since Seal -- and Activate is where the sampling those
// cells answer from is armed.
void AppendConcurrentAssertionInstall(
    const StructuralScopeLowerer& lowerer, const WalkFrame& activate_frame,
    hir::ConcurrentAssertionId id,
    const InstalledConcurrentAssertion& installed);

}  // namespace lyra::lowering::hir_to_mir
