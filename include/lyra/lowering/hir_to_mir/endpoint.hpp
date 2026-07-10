#pragma once

#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// The runtime endpoint a reader-relative reference reaches, resolved from a
// reference route. `field` is a member of the reader's own class: for a direct
// member it is the observable cell itself; for a routed reference it is the
// endpoint slot, a borrowed pointer bound in the resolve phase. `sealed`
// distinguishes the two. `cell_type` is the observable cell's type -- the field
// type directly, or the slot's pointee when sealed -- and carries the target's
// access capability (a variable cell versus a resolved-net node), so a consumer
// reads net-versus-variable from it rather than re-deriving it. One endpoint
// serves value read, value write, and change observation.
struct BoundEndpoint {
  mir::FieldId field;
  mir::TypeId field_type;
  mir::TypeId cell_type;
  bool sealed;
};

// Resolves a reference route to its endpoint on the reader's own class. Pure:
// it reads identities and types and mutates no block, so a type-level consumer
// may call it without emitting code.
[[nodiscard]] auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::ReferenceRoute& route) -> BoundEndpoint;

// The observable cell as an lvalue expression -- the read / write access
// surface: `self->cell` for a direct member, `*self->slot` for a sealed
// endpoint. Appends any sub-expressions to `frame.current_block` and returns
// the top expression unadded, the same contract as a structural field access.
[[nodiscard]] auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr;

// A borrowed pointer to the observable cell -- the change-observation
// subscription surface: the sealed slot as-is, or the address of a directly
// owned cell. Appends to `block` and returns the pointer's id.
[[nodiscard]] auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
