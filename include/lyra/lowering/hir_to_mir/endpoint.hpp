#pragma once

#include <cstdint>
#include <variant>

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

// The member a reference ends at, reached from the reader: a member of the
// class `hops` enclosing edges out -- the reader's own at zero -- or the member
// a stored route's `slot` points at, whose type is `member_type`.
struct MemberAtHops {
  mir::EnclosingHops hops;
  mir::FieldId field;
};
struct MemberThroughSlot {
  mir::FieldId slot;
  mir::TypeId member_type;
};
using MemberPlace = std::variant<MemberAtHops, MemberThroughSlot>;

// What that member is, which is a separate question from where it is: the
// cell itself, or a reference bound to it (LRM 23.3.3.2) -- a capability
// wrapper and not an address, so what reaches the cell through it comes from
// its type rather than from pointer arithmetic.
enum class MemberKind : std::uint8_t {
  kCell,
  kReference,
};

// The runtime endpoint a reader-relative reference reaches: where its member
// is, what the member is, and `cell_type`, the observable cell's type, which
// carries the target's access capability (a variable cell versus a
// resolved-net node) so a consumer reads net-versus-variable from it rather
// than re-deriving it. One endpoint serves value read, value write, and change
// observation.
struct BoundEndpoint {
  MemberPlace place;
  MemberKind kind = MemberKind::kCell;
  mir::TypeId cell_type;
};

// Resolves a value reference to its endpoint. Pure: it reads identities and
// types and mutates no block.
[[nodiscard]] auto BindEndpoint(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint;

// The observable cell as an lvalue expression -- the surface every operation on
// the cell acts through, a read, a write and a sampled read alike. A slot is
// opened to reach its member; a member that is the cell, and a reference that
// answers for one, are named directly. Appends any sub-expressions to
// `frame.current_block` and returns the top expression unadded, the same
// contract as a structural field access.
[[nodiscard]] auto EndpointCellExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::Expr;

// A borrowed pointer to the observable cell -- the change-observation
// subscription surface. A slot pointing at the cell hands it over; a reference
// is opened first, since a wait registers on the cell it binds; anything else
// takes the address of the cell it names. Appends to `block` and returns the
// pointer's id.
[[nodiscard]] auto EndpointObservablePtr(
    mir::Block& block, const WalkFrame& frame, const mir::CompilationUnit& unit,
    const BoundEndpoint& endpoint) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
