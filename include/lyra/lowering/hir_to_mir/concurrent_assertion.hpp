#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// What the design's activation has to do with the callables one concurrent
// assertion lowered to.
struct InstalledConcurrentAssertion {
  // The processes the design has to register for this assertion: the one that
  // hands each tick of its clock to the Observed region, and -- where the
  // source wrote a `disable iff` -- the one watching that condition between
  // ticks.
  std::vector<mir::CallableId> processes;
  // What a tick runs over the attempts.
  mir::CallableId step;
  // The positions of the assertion's automaton the whole property starts at,
  // a word of them at a time.
  std::vector<std::uint64_t> start;
  // The cell of the assertion's control (LRM 20.11), which decides whether an
  // attempt begins and which statement its outcome runs.
  StaticCellBinding control;
  // The cell holding the assertion's attempts.
  StaticCellBinding attempts;
};

// Lowers one concurrent assertion the scope declares into the callables it
// takes -- what a tick runs, and the process of its clock, at each tick of
// which the statement the assertion stands for is reached and its attempt
// matures, its closure made (LRM 16.14.5, 16.14.6) --
// adding each to `mir_class`. `statics` are the cells of its action block's
// static-lifetime variables, whose initializers `bring_up` runs.
[[nodiscard]] auto LowerConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const StorageBringUp& bring_up,
    const DeclaredScopes& scopes, const StaticCellBindings& statics,
    const hir::ConcurrentAssertionDecl& decl)
    -> diag::Result<InstalledConcurrentAssertion>;

// Fills the storage where the design activates: the positions the property
// starts at and what a tick runs.
// Nothing about an assertion needs an earlier phase -- what it reads is reached
// through cells sealed since Seal -- and Activate is where the sampling those
// cells answer from is armed.
void AppendConcurrentAssertionInstall(
    const StructuralScopeLowerer& lowerer, const WalkFrame& activate_frame,
    const InstalledConcurrentAssertion& installed);

// Lowers one concurrent assertion a procedure of the scope holds as a statement
// into what the scope keeps for it -- the machinery a declared one takes,
// whose attempt matures where an instance the statement queued matures
// rather than at every tick (LRM 16.14.6) -- adding each callable to
// `mir_class`. `statics` are the procedure's own, which its own lowering
// brings up.
[[nodiscard]] auto LowerProceduralConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const DeclaredScopes& scopes,
    const StaticCellBindings& statics, const hir::ProceduralBody& body,
    const hir::ConcurrentAssertion& assertion)
    -> diag::Result<InstalledConcurrentAssertion>;

// LRM 16.14.6: reaching the statement queues a pending instance on the running
// process's queue, which matures in the Observed region unless a flush point
// reaches it first, and then makes the closure of the attempt it begins,
// holding the values the statement saved (LRM 16.14.6.1).
auto LowerProceduralConcurrentAssertionStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ConcurrentAssertion& assertion, diag::SourceSpan span)
    -> diag::Result<mir::Stmt>;

// LRM 16.17: the process waits for one evaluation of the property, which its
// wait advances in the Observed region at each tick of the clock from the next
// one on, and goes on after that region once the evaluation succeeds or fails,
// running the arm the answer selects.
auto LowerExpectStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExpectStmt& expect, diag::SourceSpan span)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
