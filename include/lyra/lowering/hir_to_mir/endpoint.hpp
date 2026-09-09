#pragma once

#include <cstdint>

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

// How a member of the reader's own class stands for the observable cell it
// names. Three distinct storage shapes rather than one with a flag: the member
// may be the cell, hold a borrowed pointer to it that the resolve phase filled,
// or be a reference bound to it (LRM 23.3.3.2) -- which is a capability wrapper
// and not an address, so what reaches the cell through it comes from its type
// rather than from pointer arithmetic.
enum class EndpointReach : std::uint8_t {
  kMemberIsTheCell,
  kMemberHoldsAPointer,
  kMemberIsAReference,
};

// The runtime endpoint a reader-relative reference reaches, resolved from a
// reference route. `field` is a member of the reader's own class and `reach`
// says how that member stands for the cell. `cell_type` is the observable
// cell's type and carries the target's access capability (a variable cell
// versus a resolved-net node), so a consumer reads net-versus-variable from it
// rather than re-deriving it. One endpoint serves value read, value write, and
// change observation.
struct BoundEndpoint {
  mir::FieldId field;
  mir::TypeId cell_type;
  EndpointReach reach = EndpointReach::kMemberIsTheCell;
};

// Resolves a reference route to its endpoint on the reader's own class. Pure:
// it reads identities and types and mutates no block, so a type-level consumer
// may call it without emitting code.
[[nodiscard]] auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::ReferenceRoute& route) -> BoundEndpoint;

// The observable cell as an lvalue expression -- the surface every operation on
// the cell acts through, a read, a write and a sampled read alike. A member
// holding a pointer is opened to reach it; a member that is the cell, and a
// reference that answers for one, are named directly. Appends any
// sub-expressions to `frame.current_block` and returns the top expression
// unadded, the same contract as a structural field access.
[[nodiscard]] auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr;

// A borrowed pointer to the observable cell -- the change-observation
// subscription surface. A member already holding one hands it over; anything
// else takes the address of the cell it names. Appends to `block` and returns
// the pointer's id.
[[nodiscard]] auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
