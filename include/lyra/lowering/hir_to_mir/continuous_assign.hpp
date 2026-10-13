#pragma once

#include <functional>
#include <optional>
#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers a continuous assignment fully, including any Resolve- and
// Initialize-phase side effects the LHS's type demands. A variable-typed
// LHS produces a body coroutine that writes the cell directly every
// iteration (LRM 10.3.2). A resolved-net-typed LHS installs a driver-handle
// field on the enclosing class, attaches an independent driver at Resolve,
// and the body updates the driver every iteration; the net resolves its
// drivers, so several assignments to one net each install their own driver
// (LRM 6.5). A drive of a whole net from the whole of a variable or a net of
// its type also tells its driver so at Resolve, and its body first asks
// whether the net now shows that object, which leaves it nothing to do. A
// drive carrying a delay (LRM 10.3.3) writes through a timer instead: each new
// value is scheduled, replacing the one pending, and the timer's firing writes
// it. The write protocol is picked by the LHS's MIR type; callers see one API
// for both cases. The three phase frames all address the same enclosing class;
// each carries its phase's target block.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const hir::ContinuousAssign& src) -> diag::Result<mir::CallableDecl>;

// What a continuous drive writes, named in the body it is asked for: the
// drive is stated once and written into its Resolve step, its Initialize seed
// and its own body, each a body of its own.
using DriveDestination = std::function<diag::Result<Lvalue>(const WalkFrame&)>;

// The delays a drive is written with (LRM 10.3.3), and the entry that picks
// among several of them by the value a change is to: a scalar net's rule (LRM
// 28.16) or a vector net's.
struct DriveDelay {
  hir::AssignmentDelay delay;
  support::BuiltinFn propagation_delay;
};

// The same lowering over a destination its caller names, for a drive whose
// target the source did not write as an expression of this scope.
auto LowerContinuousDrive(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const DriveDestination& destination, const hir::Expr& source,
    support::StrengthLevel drive_strength,
    const std::optional<DriveDelay>& delay,
    std::span<const hir::SensitivityEntry> sensitivity)
    -> diag::Result<mir::CallableDecl>;

}  // namespace lyra::lowering::hir_to_mir
