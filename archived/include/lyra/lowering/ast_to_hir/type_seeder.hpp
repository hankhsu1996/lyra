#pragma once

namespace lyra::lowering::ast_to_hir {

struct BodyLoweringInput;
struct Context;

// Phase 0 body-type seeding prepass.
//
// Interns all body-reachable types from the Phase 1 body frontier into the
// Phase 0 TypeArena. Called once per specialization group before Phase 1 body
// lowering starts.
//
// This is structural type completion, not pseudo-lowering. It walks
// type-bearing AST constructs (expression result types, body-local variable
// types, subroutine signatures) without building HIR. Phase 1 remains the
// authority for user-visible diagnostics.
void SeedBodyTypes(const BodyLoweringInput& input, Context* ctx);

}  // namespace lyra::lowering::ast_to_hir
