#pragma once

#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/method.hpp"

namespace lyra::lowering::hir_to_mir {

// Stable identity of a resolved-net target reached by a continuous
// assignment's LHS. Two assignments whose LHS reaches the same net share
// one identity value (LRM 6.5): a direct member of the driver's own scope is
// keyed by its data-object id, a routed target by its resolved-slot id.
using ContinuousWriteTarget =
    std::variant<hir::StructuralDataObjectId, hir::RoutedRefId>;

// Caller-owned record of the resolved-net targets a lowering loop has
// already installed a driver on. `LowerContinuousAssign` inspects and
// appends to it: a duplicate at insertion is a same-scope multi-driver
// (LRM 6.5), which the lowering rejects with a user diagnostic. Callers
// for whom multi-driver is impossible by construction pass a fresh
// instance and never inspect it.
using ContinuousAssignDrivenNets = std::vector<ContinuousWriteTarget>;

// Lowers a continuous assignment fully, including any Resolve- and
// Initialize-phase side effects the LHS's type demands. A variable-typed
// LHS produces a body coroutine that writes the cell directly every
// iteration (LRM 10.3.2). A resolved-net-typed LHS installs a driver-handle
// field on the enclosing class, attaches the driver at Resolve, seeds it
// at Initialize, and the body updates the driver every iteration (LRM
// 6.5). The write protocol is picked by the LHS's MIR type; callers see
// one API for both cases. The three phase frames all address the same
// enclosing class; each carries its phase's target block. `driven_nets` is
// the caller's dedup set: the lowering registers the LHS's target into it
// and returns `Fail` on a duplicate.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    std::string name, const hir::ContinuousAssign& src,
    ContinuousAssignDrivenNets& driven_nets) -> diag::Result<mir::MethodDecl>;

}  // namespace lyra::lowering::hir_to_mir
