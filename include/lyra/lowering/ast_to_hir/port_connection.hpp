#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace slang::ast {
class Scope;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Synthesizes the implied continuous assignments for every instance port
// connection in `slang_scope`. A variable input or output port connection is a
// continuous assignment between the two objects' own storage, source -> sink by
// direction (LRM 23.3.3, 23.3.3.2); the child-side endpoint resolves through a
// cross-unit reference, the same path a hierarchical reference uses
// (reference_resolution.md). Input writes the child port from the parent-side
// source; output writes the parent target from the child port. This runs after
// every variable and instance binding in the scope exists, so a connection
// resolves regardless of the instantiation-vs-declaration source order. `ref`
// ports, `inout` / net ports, non-integral ports, unconnected ports, and
// instance-array port connections are rejected so they never lower into a
// silently-wrong shape.
auto LowerScopePortConnectionsInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::Scope& slang_scope)
    -> diag::Result<void>;

}  // namespace lyra::lowering::ast_to_hir
